# flexiprocity

[reciprocity.io](https://reciprocity.io) is a website where you can indicate
interest in people that they only find out about if it's reciprocated.

[flexiprocity](https://reciprocity.rpm.cc) is inspired by reciprocity, with a
different feature set and entirely fresh implementation. The site calls itself
reciprocity still, but you can refer to this particular version of it as
flexiprocity.

New features that are implemented:

- Attempt to sort newer users to near the top of the list, so you don't have to
  reread the whole thing,
- Account self-deletion.
- User-customisable columns.

## Current roadmap

I can't implement Facebook integration unless I can attach the app to a
"verified business account". Verification is only available to business accounts
that go through a review process that involves your page existing for a while
and having engagement with customers. I don't particularly fancy bothering with
it for now, so I'm stuck until someone else can offer to host the app under
their business account.

This is a pretty serious problem! Facebook integration is kind of the whole
point of the app. In order to avoid having nothing to show for my work, I
implemented Google login, but Google of course doesn't know who is friends with
who, so this isn't worth much.

This has me looking to other major social networks for alternative integrations.
The level of Twitter API access that allows access to your followers costs $100
a month, so that's shelved for now. My hope is that the equivalent on Bluesky is
free. If so, that would probably be enough to launch on.

## Contributing

One thing I'd particularly appreciate is proposed aesthetic improvements. You
can of course do these as PRs, but if you don't know how to or don't want to,
showing me HTML+CSS mockups would be great, or just describing what you want in
words or pictures is good too.

### Testing locally

#### Requirements

The version I use is in parentheses, but I expect some other versions will be
compatible.

- PostgreSQL (16)
- GHC (9.6) and cabal-install (3.12.1.0)
- Elm compiler (0.19.1)
- nginx (1.26.2)
- npm (10.8.2)
- ssh-keygen (part of OpenSSH, I'm sure any reasonable version is fine)

#### Setting up PostgreSQL

Create `secrets/seeds.sql`. You can leave it blank for now.

Create the local users `api`, `inbox`, and `meddler`. I set them up to be able
to log in via unix socket only, using peer authentication and
[`pg_ident.conf`](https://www.postgresql.org/docs/current/auth-username-maps.html)
so I can run everything as my local user. You may be able to get password
authentication working with judicious application of the appropriate environment
variables, but that's up to you for now.

Run `schema/apply.sh` as the superuser (set the `PGUSER` environment variable if
the superuser is not `postgres`).

#### Setting up authentication

Run `secrets/new-jwt-key.sh`.

Create `secrets/friendica_instances.json` as an empty JSON object (a file
containing only `{}`).

Create `secrets/google_client_secret.json` as a JSON object
`{"web":{"client_id":"","client_secret":""}}`.

You should now be able to run the `auth-server` process. Try
`cd auth-server; cabal run auth-server` to confirm.

However, the auth server doesn't have any authentication methods set up yet. Let's skip that for now.

#### Setting up nginx

The most annoying part of this is getting an SSL certificate set up that your
browser trusts. I do this by owning and managing `rpm.cc` in Amazon Route53,
using `certbot` to issue a wildcard cert for that domain, and then configuring
my laptop to map `local.rpm.cc` to `localhost`. Another possibility could be to
use [Tailscale](https://tailscale.com/) along with `tailscale cert` to get an
SSL cert for your local device. A third possibility could be a self-signed cert
that you persuade your browser to trust somehow.

However you do it, replace the `server_name` in
`nginx/flexiprocity.conf.template` with the hostname you have set up, and put
the certificates in `secrets/ssl/fullchain.pem` and `secrets/ssl/privkey.pem`.

Then hopefully `cd nginx; ./start.sh` should work. If you don't have
`inotifywait`, you might need to modify the script, sorry.

#### Setting up postgraphile

Hopefully `cd postgraphile; ./start.sh dev` should work. If it doesn't, the most
likely reason is that you have a PostgreSQL setup that's different from mine.
Good luck!

#### Setting up the frontend

Again, I hope that `cd frontend; ./build.sh` should just work here; again if you
don't have `inotifywait` you might have to make some changes.

#### Putting it all together

This is what `./dev-tmux.sh` does in the root directory, if you have tmux
installed.

## Terms of Service compliance notes

These are the points from the terms of services that I signed up for that seemed
worth recording.

### Facebook

- There's some restrictions on sharing platform data, but my guess is that
  sharing checkbox data (as may be necessary for a migration from one
  reciprocity to another) counts as its own thing, not as platform data.
- We must make reasonable efforts to keep Platform Data up to date, including
  modifications and deletions, and we must allow users to ask for their data to
  be modified or deleted.
- We have to delete platform data under a variety of circumstances; "whenever
  it's not needed, or whenever requested" should probably suffice.
- You need to report security incidents to them.
- They can audit you, and "You will cooperate with the Audits, including by (1)
  providing all necessary physical and remote access to your IT Systems and
  Records" (!), and "If an Audit reveals any non-compliance by you or your
  Service Provider(s) then you will reimburse us for all of our reasonable costs
  and expenses associated with conducting the Audit and any related follow-up
  Audits."
- Their licenses for your content seem kind of intense, and survive even when
  you stop using the platform.

### Twitter

(N.B. that I shelved implementing sign in with Twitter, because the API for
fetching user followers is $100/month.)

- The Sign in with Twitter option must be displayed at least as prominently as
  any other sign-up or sign-in feature on your service. You must also provide
  people without a Twitter account the opportunity to create one via Twitter.
- Once someone on your service authenticates via Sign in with Twitter you must
  clearly display their Twitter identity. Twitter identity includes the person’s
  current Twitter @handle, avatar, and Twitter logo. Any display of someone’s
  Twitter followers on your service must clearly show that the relationship is
  associated with Twitter.
