# flexiprocity

[reciprocity.io](https://reciprocity.io) is a website where you can indicate
interest in people that they only find out about if it's reciprocated.

flexiprocity is inspired by reciprocity, with a different feature set and
entirely fresh implementation.

## ideas box

- you can sort people by when they became visible to you
- checkboxes are private, public, or reciprocal, meaning that you checking
  someone is visible never, always, or when they check you respectively
- you can check any box at any level of publicity, and make your own boxes
- you can see statistics about which boxes are generally ticked most frequently
- you can tag yourself, and search by tag (key-value tags? showing you the
  things people most commonly choose?)
- some kind of technical countermeasures to make it harder for the platform
  operator to spy on the users?

## things from Facebook ToS to ensure

- Prohibited from "Processing friend lists from Facebook to establish social
  connections in your App unless each person in that connection has granted you
  access to that information for that purpose", so we'd need to ensure we had
  that.
- There's some restrictions on sharing platform data, but my guess is that
  sharing checkbox data (as may be necessary for a migration from one
  reciprocity to another) counts as its own thing, not as platform data.
- We must make reasonable efforts to keep Platform Data up to date, including
  modifications and deletions, and we must allow users to ask for their data to
  be modified or deleted.
- We have to delete platform data under a variety of circumstances; "whenever
  it's not needed, or whenever requested" should probably suffice.
- [privacy policy requirements](https://developers.facebook.com/terms/#privacypolicy)
- "You must have a publicly available way for people to report security
  vulnerabilities in your App to you, and you must promptly address identified
  deficiencies."
- You need to report security incidents to them.
- They can audit you, and "You will cooperate with the Audits, including by (1)
  providing all necessary physical and remote access to your IT Systems and
  Records" (!), and "If an Audit reveals any non-compliance by you or your
  Service Provider(s) then you will reimburse us for all of our reasonable costs
  and expenses associated with conducting the Audit and any related follow-up
  Audits."
- I probably need to read the GDPR huh


### irrelevant ToS commentary

- their licenses for your content seem kind of intense, and survive even when
  you stop using the platform
