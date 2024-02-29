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

## Contributing

### Testing on the live site

Until I attach flexiprocity to a "verified business account", users need to:

- have a ["Meta Developer Account"][1],
- be manually added to the app configuration by me,
- confirm this addition in their [developer requests][2] page.

[1]: https://developers.facebook.com/
[2]: https://developers.facebook.com/settings/developer/requests/

Please feel free to message, e-mail or open a GitHub issue with any feedback you
have.

One thing I'd particularly appreciate is proposed aesthetic improvements. You
can of course do these as PRs, but if you don't know how to or don't want to,
showing me HTML+CSS mockups would be great, and just describing what you want in
words or pictures is good too.

### Testing locally

You'll need a Facebook app ID and secret, and you can't use mine. I assume most
people won't bother with this, but let me know if you want instructions.

One thing I will remark upon, because it could be useful in other contexts:
Facebook will refuse to run the login process unless your site is using HTTPS.
I guess you could try to set up an SSL cert that is trusted by your browser for
the localhost domain, but it seemed easier to me to just set up an alias in my
laptop's network configuration for `local.rpm.cc` to `localhost` and put a
wildcard cert for `*.rpm.cc` in my configuration. Even though you can't prove
your ownership of the domain with a local-only hosted site, once you have the
cert you can use it with any host that resolves to the domain.

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
