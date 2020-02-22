firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Main.init({
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
			data.expires = new Date(data.expires).toISOString().slice(0, 10);

			sets[doc.id] = data;
		}

		app.ports.allSets.send(Object.values(sets));
	});

db.collection('votes')
	.onSnapshot(snap => {
		let votes = [];
		for (let i = 0; i < snap.size; i++) {
			const data = snap.docs[i].data();
			for (const vote of data.routes)
				votes.push({ set_id: data.set_id, ...vote });
		}

		app.ports.allVotes.send(votes);
	});
