const express = require('express');
const app = express();
const path = require('path');

// Config
const port = 3000;
console.log(path.join(__dirname + '/public/index.html'))
app.get('/', function(req, res) {
  res.sendFile(path.join(__dirname + '/public/index.html'))
});

app.use(express.static('public'));

app.listen(port, function () {
  console.log('Listening on ' + port);
});
