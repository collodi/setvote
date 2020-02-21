firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Main.init({
	node: document.getElementById('elm')
});

app.ports.castVote.subscribe(data => {
	console.log(data);

	db.collection('votes').add(data);
});

db.collection('sets')
	.orderBy('created', 'desc')
	.where('expires', '>', Date.now())
	.limit(1)
	.get()
	.then(snap => {
		if (snap.empty) {
			app.ports.openSet.send({});
			return;
		}

		let data = snap.docs[0].data();
		data.id = snap.docs[0].id;
		data.expires = new Date(data.expires).toISOString().slice(0, 10);

		app.ports.openSet.send(data);
	});
