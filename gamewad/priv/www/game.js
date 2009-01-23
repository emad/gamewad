var GAMESLUG = parseQueryString(window.location.search).slug;
if (typeof(EMBED) == 'undefined') {
    EMBED = createDOMFunc('embed');
}

function start_get_game_by_slug(slug) {

    return doXHR("/gamejson/" + slug, {
        method: 'GET',
        mimeType: 'text/plain',
        headers: {
            Accept: 'application/json',
            'Content-Type': 'application/x-www-form-urlencoded'
        }
    }).addCallback(evalJSONRequest);
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

function show_game(obj) {
  if (!obj) { return; }

  var s;
  if (obj.use_local) {
    s = "/static/games/" + obj.game.slug + "/" + obj.game.swf_url.replace(/^.*[\/\\]/g, '');
      }
  else { s = obj.game.swf_url};
  if (obj.is_favorite) {
    show_delete_favorite();
      }
  else {
    show_add_favorite();
  }
  replaceChildNodes('game_name', obj.game.name);
  replaceChildNodes('game_description', obj.game.description);
  var flashvars = {};
  var params = {allowscriptaccess: "never", menu: "false", quality: "high", wmode: "opaque"};
  var attributes = {};
  swfobject.embedSWF(s, "game_embed", obj.game.width, obj.game.height, '9', "/expressInstall.swf", flashvars, params, attributes) ;
}

function show_delete_favorite() {
  hideElement("add_favorite");
  showElement("delete_favorite");
}

function show_add_favorite() {
  hideElement("delete_favorite");
  showElement("add_favorite");

}

function show_random_games(arr) {
  if (!arr) { return; }
  var games = map(function (obj) {
       return A({"href": "/game.html?slug=" + obj.slug},
		IMG({"class": "thumbnail",
		      "src": obj.thumbnail_url, "title": obj.name}))}, arr);
  replaceChildNodes("random_games", games);
 
}
	    
function get_slug() {
  return GAMESLUG;
}


function add_favorite(slug) {
  if (!slug) { slug = get_slug() };
  do_favorite("add", slug).addCallback(show_delete_favorite);
}

function delete_favorite(slug) {
  if (!slug) { slug = get_slug() };
  do_favorite("delete", slug).addCallback(show_add_favorite);
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

function game_loaded() {
  start_get_random_games().addCallback(show_random_games);
  var SLUG = get_slug();
  if (SLUG) {
    start_get_game_by_slug(SLUG).addCallback(show_game);
  }

}

addLoadEvent(game_loaded);
