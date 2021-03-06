const functions = require('firebase-functions');

const admin = require('firebase-admin');
admin.initializeApp();

const db = admin.firestore();

function escapeGrade(grade) {
	return grade.replace(/\./g, '!').replace(/\//g, '@');
}

exports.aggregateVotes = functions.firestore
	.document('votes/{vote}')
	.onWrite(async (change, ctx) => {
		const data = change.after.data();

		const set_id = data.set_id;
		const route = data.route;
		const grade = escapeGrade(data.grade);

		const pollId = set_id + '-' + route;
		const pollRef = db.collection('polls').doc(pollId);

		await db.runTransaction(async (trans) => {
			const pollDoc = await trans.get(pollRef);
			const cnt = pollDoc.get(grade) || 0;

			trans.set(pollRef, { [grade]: cnt + 1 }, { merge: true });
		});
	});

exports.aggregateFavs = functions.firestore
	.document('favs/{fav}')
	.onWrite(async (change, ctx) => {
		const data = change.after.data();

		const set_id = data.set_id;
		const route = data.route;

		const pollId = set_id + '-' + route;
		const pollRef = db.collection('polls').doc(pollId);

		await db.runTransaction(async (trans) => {
			const pollDoc = await trans.get(pollRef);
			const cnt = pollDoc.get('fav') || 0;

			trans.set(pollRef, { fav: cnt + 1 }, { merge: true });
		});
	});
