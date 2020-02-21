firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Main.init({
	node: document.getElementById('elm')
});

app.ports.addSet.subscribe(data => {
	// extra string append makes it local time
	data.expires = Date.parse(data.expires + 'T00:00:00');
	db.collection('sets').add(data);
});

let sets = {};
db.collection('sets')
	.onSnapshot(snap => {
		sets = {};
		for (let i = 0; i < snap.size; i++) {
			const doc = snap.docs[i];

			let data = doc.data();
			data.expires = new Date(data.expires).toISOString().slice(0, 10);
			sets[doc.id] = data;
		}

		// gatherVotes();
		console.log(Object.values(sets));
		app.ports.allSets.send(Object.values(sets));
	});

function gatherVotes() {
	db.collection('votes').get()
		.then(snap => {
			// group votes by id
			for (let i = 0; i < snap.size; i++) {
				const data = snap.docs[i].data();
			}
		});
}
