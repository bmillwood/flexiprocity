const facebookEnabled = true;
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
const app = Elm.Main.init({
    flags: {
        latestPrivacyPolicy,
        facebookEnabled,
        googleEnabled: false
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
    }
});
