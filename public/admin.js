firebase.initializeApp(firebaseConfig);

var db = firebase.firestore();

var app = Elm.Admin.init({
	node: document.getElementById('elm')
});

app.ports.addSet.subscribe(data => {
	// extra string append makes it local time
	data.expires = Date.parse(data.expires + 'T00:00:00');
	data.created = Date.now();

	db.collection('sets').add(data);
});

app.ports.deleteSet.subscribe(set_id => {
	db.collection('sets').doc(set_id).delete();

	db.collection('votes')
		.where('set_id', '==', set_id)
		.get().then(snap => {
			let batch = db.batch();
			snap.forEach(doc => {
				batch.delete(doc.ref);
			});
			batch.commit();
		});
});

db.collection('sets')
	.orderBy('created', 'desc')
	.onSnapshot(snap => {
		let sets = {};
		for (let i = 0; i < snap.size; i++) {
			const doc = snap.docs[i];

			let data = doc.data();
			data.id = doc.id;
			data.showDelete = false;
			data.expires = new Date(data.expires).toISOString().slice(0, 10);

			sets[doc.id] = data;
		}

		app.ports.allSets.send(Object.values(sets));
	});

function unescapeGrade(grade) {
	return grade.replaceAll('!', '.').replaceAll('@', '/');
}

function pollToList(poll) {
	const keys = Object.keys(poll);
	let vals = Array.from(keys).fill(0);

	for (const k in poll) {
		const i = keys.indexOf(k);
		vals[i] = poll[k];
	}

	return { grades: keys.map(unescapeGrade), counts: vals };
}

db.collection('polls')
	.onSnapshot(snap => {
		let polls = [];
		for (let i = 0; i < snap.size; i++) {
			const id = snap.docs[i].id;
			const [set_id, route] = id.split('-');

			const data = snap.docs[i].data();
			const fav = data.fav || 0;
			delete data.fav;

			polls.push({ set_id, route, fav, ...pollToList(data) });
		}

		app.ports.allPolls.send(polls);
	});

firebase.auth()
	.onAuthStateChanged(user => {
		app.ports.authd.send(!!user);
	});
