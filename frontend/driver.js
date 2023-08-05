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
            payload: response
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
                payload: response
            });
        }, {scope: 'public_profile,user_friends'});
        break;
    case 'facebook-friends':
        FB.api(
            `/${request.userId}/friends`,
            'GET',
            {},
            function(response) {
                console.log('friends', response);
                app.ports.receiveFromJS.send({
                    kind: 'facebook-friends',
                    payload: response
                });
            }
        );
    }
});
