firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Login.init({
	node: document.getElementById('elm')
});

function login(email, pwd) {
	firebase.auth()
		.signInWithEmailAndPassword(email, pwd)
		.then(user => {

		})
		.catch(err => {
			// TODO
		});
}

function signup(email, pwd) {
	firebase.auth()
		.createUserWithEmailAndPassword(email, pwd)
		.then(user => {

		})
		.catch(err => {
			// TODO
			console.log(err);
		});
}

app.ports.cmd.subscribe(data => {
	if (data.action === 'login')
		login(data.email, data.password);
	else if (data.action === 'signup')
		signup(data.email, data.password);
});
