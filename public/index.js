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

var db = firebase.firestore();

var app = Elm.Vote.init({
	node: document.getElementById('elm')
});

app.ports.castVote.subscribe(data => {
	data.user_key = user_key;

	const doc_id = [data.user_key, data.set_id, data.route].join('-');
	db.collection('votes').doc(doc_id).set(data);
});

function subscribeToVotes(set_id) {
	db.collection('votes')
		.where('set_id', '==', set_id)
		.where('user_key', '==', user_key)
		.onSnapshot(snap => {
			let arr = [];
			for (let i = 0; i < snap.size; i++)
				arr.push(snap.docs[i].data().route);

			app.ports.votedRoutes.send(arr);
		});
}

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

		subscribeToVotes(data.id);

		app.ports.openSet.send(data);
	});
