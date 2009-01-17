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
  else { s = obj.game.swf_url}
  replaceChildNodes('game_name', obj.game.name);
  replaceChildNodes('game_description', obj.game.description);
  var flashvars = {};
  var params = {allowscriptaccess: "never", menu: "false", quality: "high", wmode: "opaque"};
  var attributes = {};
  swfobject.embedSWF(s, "game_embed", obj.game.width, obj.game.height, '9', "/expressInstall.swf", flashvars, params, attributes) ;
}

function show_random_games(arr) {
  if (!arr) { return; }
  var games = map(function (obj) {
       return A({"href": "/game.html?slug=" + obj.slug},
 	    IMG({"src": obj.thumbnail_url}))}, arr);
  replaceChildNodes("random_games", games);
 
}
	    
function get_slug() {
  return GAMESLUG;
}

function game_loaded() {
  start_get_random_games().addCallback(show_random_games);
  var SLUG = get_slug();
  if (SLUG) {
    start_get_game_by_slug(SLUG).addCallback(show_game);
  }

}

addLoadEvent(game_loaded);
