function customUUID() {
	const r1 = Math.random() * 1000 + 1;
	const r2 = Math.random() * 1000 + 1;

	return r1.toString(36) + r2.toString(36)
		+ Date.now().toString(36);
}

function getUserKey() {
	if (localStorage.getItem("USER_KEY") === null)
		localStorage.setItem("USER_KEY", customUUID());

	return localStorage.getItem("USER_KEY");
}

const user_key = getUserKey();

firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Vote.init({
	node: document.getElementById('elm')
});

app.ports.castVote.subscribe(data => {
	data.user_key = user_key;
	db.collection('votes').add(data);
});

db.collection('votes')
	.where('user_key', '==', user_key)
	.onSnapshot(snap => {
		let arr = [];
		for (let i = 0; i < snap.size; i++)
			arr.push(snap.docs[i].data().set_id);

		app.ports.votedSets.send(arr);
	});

db.collection('sets')
	.orderBy('expires')
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
