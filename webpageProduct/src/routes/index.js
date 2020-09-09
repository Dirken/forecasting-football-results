const { Router } = require('express');
const router = Router();
const admin = require('firebase-admin');

var serviceAccount = require("../../referee-36806-firebase-adminsdk-hecn4-ade7dc2729.json")

admin.initializeApp({
    credential: admin.credential.cert(serviceAccount),
    databaseURL: 'https://referee-36806.firebaseio.com/'
});

const db = admin.database();


router.get('/', (req, res) => {
    res.render('homePage');
});

router.get('/leagues', (req, res) => {
    res.render('leagues');
});

router.get('/services', (req, res) => {
    res.render('services');
});

router.get('/about', (req, res) => {
    res.render('about');
});

router.get('/login', (req, res) => {
    res.render('login');
});

router.get('/contact', (req, res) => {
    res.render('contact');
});


//render data
router.get('/contact', (req, res) => {
    db.ref('contacts').once('value', (snapshot) => {
        const data = snapshot.val();
        res.render('index', { contacts : data });
    });
});



//delete contacts
router.get('/delete-contact/:id', (req, res) => {
    db.ref('contacts/' + req.params.id).remove();
    res.redirect('/');
})

//add contacts
router.post('/new-contact', (req, res) =>{
    const newContact = {
        firstName: req.body.firstname,
        lastName: req.body.lastname,
        email: req.body.email,
        phone: req.body.phone
    }
    db.ref('contacts').push(newContact);
    res.redirect('/');
})


module.exports = router;

