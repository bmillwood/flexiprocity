const facebookEnabled = false;
if(facebookEnabled) {
    window.fbAsyncInit = function() {
        FB.init({
            appId            : '195604956830336',
            autoLogAppEvents : true,
            xfbml            : false,
            version          : 'v17.0'
        });
        FB.getLoginStatus(function(response) {
            console.log('status', response);
            app.ports.receiveFromJS.send({
                kind: 'facebook-login-status',
                response
            });
        });
    };
    const fbScript = document.createElement("script");
    fbScript.async = true;
    fbScript.defer = true;
    fbScript.crossOrigin = "anonymous";
    fbScript.src = "https://connect.facebook.net/en_US/sdk.js";
    fbScript.onerror = function(error) {
        app.ports.receiveFromJS.send({
            kind: 'sdk-load-failure',
            which: 'facebook'
        });
    };
    document.body.appendChild(fbScript);
}

var websocket;

const app = Elm.Main.init({
    flags: {
        latestPrivacyPolicy,
        facebookEnabled,
        googleEnabled: true
    }
});
app.ports.sendToJS.subscribe(function(request) {
    switch(request.kind) {
    case 'facebook-login':
        FB.login(function(response) {
            console.log('login', response);
            app.ports.receiveFromJS.send({
                kind: 'facebook-login-status',
                response
            });
        }, {scope: 'public_profile,user_friends,user_link'});
        break;
    case 'facebook-logout':
        FB.logout(function(response) {
            console.log('logout', response);
            app.ports.receiveFromJS.send({
                kind: 'facebook-login-status',
                response
            });
        });
        break;
    case 'facebook-api':
        FB.api(
            request.path,
            'GET',
            request.params,
            function(response) {
                console.log('api', { request, response });
                app.ports.receiveFromJS.send({
                    kind: 'facebook-api',
                    request,
                    response
                });
            }
        );
        break;
    case 'sentry':
        window.Sentry && Sentry.captureMessage(request.message);
        break;
    case 'connect-websocket':
        if(websocket) {
            websocket.close(1000, "re-opening websocket");
            websocket = null;
        }
        websocket = new WebSocket("/graphql", "graphql-transport-ws");
        websocket.addEventListener("open", function (ev) {
            console.log(ev);
            ev.target.send(JSON.stringify({type: "connection_init", payload: {}}));
        });
        websocket.addEventListener("close", function(ev) {
            console.log(ev);
            if(ev.target == websocket) {
                app.ports.receiveFromJS.send({
                    kind: "websocket-closed",
                    code: ev.code,
                    reason: ev.reason
                });
            } else {
                // don't want to prompt the frontend to try to reconnect
                console.log("close event is for an old websocket, discarding");
            }
        });
        websocket.addEventListener("message", function(ev) {
            console.log(ev);
            app.ports.receiveFromJS.send({
                kind: "websocket-message",
                message: JSON.parse(ev.data)
            });
        });
        break;
    case 'send-websocket':
        websocket.send(JSON.stringify(request.message));
        break;
    case 'disconnect-websocket':
        websocket.close(1000, "frontend requested");
        break;
    }
});
