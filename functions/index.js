const functions = require('firebase-functions');

const admin = require('firebase-admin');
admin.initializeApp();

const db = admin.firestore();

exports.aggregateVotes = functions.firestore
	.document('votes/{vote}')
	.onWrite(async (change, ctx) => {
		const data = change.after.data();

		const set_id = data.set_id;
		const route = data.route;
		const grade = data.grade;

		const pollId = set_id + '-' + route;
		const pollRef = db.collection('polls').doc(pollId);

		await db.runTransaction(async (trans) => {
			const pollDoc = await trans.get(pollRef);
			const cnt = pollDoc.get(grade) || 0;

			trans.set(pollRef, { [grade]: cnt + 1 }, { merge: true });
		});
	});
