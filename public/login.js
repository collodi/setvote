firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Login.init({
	node: document.getElementById('elm')
});

function login(email, pwd) {
	firebase.auth()
		.signInWithEmailAndPassword(email, pwd)
		.catch(err => {
			app.ports.message.send(err.message);
		});
}

function signup(email, pwd) {
	firebase.auth()
		.createUserWithEmailAndPassword(email, pwd)
		.then(userCred => {
			const user = userCred.user;
			db.collection('users')
				.doc(user.uid)
				.set({ uid: user.uid })
				.catch(err => {
					app.ports.message.send(err.message);
				});
		})
		.catch(err => {
			app.ports.message.send(err.message);
		});
}

app.ports.cmd.subscribe(data => {
	if (data.action === 'login')
		login(data.email, data.password);
	else if (data.action === 'signup')
		signup(data.email, data.password);
	else if (data.action === 'signout')
		firebase.auth().signOut();
});

firebase.auth().onAuthStateChanged(user => {
	app.ports.authd.send(!!user);
});
