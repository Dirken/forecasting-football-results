const express = require('express');
const app = express();
const morgan = require('morgan'); 
const exphbs = require('express-handlebars');
const path = require('path');
var nodemailer = require('nodemailer'); 

//var sendmail = require('./routes/sendMail');

//Settings
app.set('port', process.env.PORT || 3000);
app.set('views', path.join(__dirname, 'views'));
app.engine('.hbs', exphbs({
    defaultLayout: 'main',
    extname: '.hbs'

}));

app.set('view engine', '.hbs');

//middleware.
app.use(morgan('dev'));
app.use(express.urlencoded({extended: false}));

//routes
app.use(require('./routes/index.js'));
//app.use('/sendmail', sendmail);

//static files
app.use(express.static(path.join(__dirname, 'public')));





module.exports = app;