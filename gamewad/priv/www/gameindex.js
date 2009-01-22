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

function start_get_favorites() {
    return doXHR("/user/favorites/get", {
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
  var j; 
  var obj;
  for (i=0; i < arr.length; i++) {
    obj = arr[i];
    games.push(A({"href": "/game.html?slug=" + obj.slug},
		 IMG({"class": "thumbnail", "src": obj.thumbnail_url})));
    if ((i+1) % 3 == 0) { games.push(BR()); games.push(BR()); };
  }
  replaceChildNodes("random_games", games);
}

function show_favorites(arr) {
  if (!arr) { return; }
  var i = 0;
  var games = [];
  var s;
  for (i=0; i < arr.length; i++) {
    obj = arr[i];
    s = SPAN({"style" :"position: relative", "id": obj.slug + "_favorite"},
	     SPAN({"class": "deletoid"},
		  A({"href": "javascript:delete_favorite('"
			+ obj.slug +"')"},
		    IMG({"src": "/static/img/delete.gif"}))),
	     A({"href": "/game.html?slug=" + obj.slug},
	       IMG({"src": obj.thumbnail_url}))
	     );

    //   s.push(A({"href": "/game.html?slug=" + obj.slug},
    //		IMG({"src": obj.thumbnail_url})));
    games.push(s);
  }
  replaceChildNodes("favorite_games", games);
} 

function do_favorite(action, slug) {
  return doXHR("/user/favorites/" + action + "/" + slug, {
    method: 'POST',
        mimeType: 'text/plain',
        headers: {
      Accept: 'application/json',
	  'Content-Type': 'application/x-www-form-urlencoded'
	  }
    });
    }

function delete_favorite(slug) {
  do_favorite("delete", slug);
  hideElement(slug + "_favorite");
}


function game_loaded() {
  start_get_random_games().addCallback(show_random_games);
  start_get_favorites().addCallback(show_favorites);
}

addLoadEvent(game_loaded);
