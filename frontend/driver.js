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
const app = Elm.Main.init({
    // flags: {}
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
        }, {scope: 'public_profile,user_friends'});
        break;
    case 'facebook-api':
        FB.api(
            request.path,
            'GET',
            {},
            function(response) {
                console.log('api response', request.id, response);
                app.ports.receiveFromJS.send({
                    kind: 'facebook-api',
                    id: request.id,
                    response
                });
            }
        );
        break;
    }
});
