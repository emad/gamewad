if (typeof(EMBED) == 'undefined') {
    EMBED = createDOMFunc('embed');
}

function start_get_random_games() {
  return doXHR("/random_games/9", {
              method: 'GET',
        mimeType: 'text/plain',
        headers: {
            Accept: 'application/json',
            'Content-Type': 'application/x-www-form-urlencoded'
        }
    }).addCallback(evalJSONRequest);
}


function show_random_games(arr) {
  if (!arr) { return; }
  var games = [];
  var i = 0;
  var j 
  var obj;
  for (i=0; i < arr.length; i++) {
    obj = arr[i];
    games.push(A({"href": "/game.html?slug=" + obj.slug},
		IMG({"src": obj.thumbnail_url})));
    if ((i+1) % 3 == 0) { games.push(BR()); games.push(BR()); };
  }
  replaceChildNodes("random_games", games);
 
}


function game_loaded() {
  start_get_random_games().addCallback(show_random_games);
}

addLoadEvent(game_loaded);
