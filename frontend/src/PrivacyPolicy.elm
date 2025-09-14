module PrivacyPolicy exposing (version, view)

import Crypto.Hash
import Html exposing (Html)
import Html.Attributes as Attributes
import Markdown.Parser
import Markdown.Renderer

version : String
version = Crypto.Hash.sha256 markdown |> String.left 16

markdown : String
markdown = """
## Privacy policy

Below are detailed the kinds of data collected for the app,
and the purposes for which it is collected. The purposes are intended
to be exhaustive: no data is collected or can be used for any other
purpose without your express prior consent.

To avoid changing the privacy policy too often, I may list
data collected for a purpose even if that purpose is not implemented
yet or that data is not collected yet. Agreeing to the privacy policy
nevertheless grants me permission to collect and use the information
listed for the purposes listed in future.

The app uses data provided directly by you:
* Your bio, to display to other users according to your visibility settings,
* Your preferences and settings, in order to customise your experience of the app.

If you log in with Google, the app receives and stores from them:
* Your email address, which is used as your account identifier and to send match emails,
* Your name and profile picture, to show to other users of the app according to your visibility settings.

If you log in with Facebook, the app uses data from Facebook as follows:
* Your name and profile picture, to show to other users of the app according to your visibility settings,
* The list of your friends who use the app, to determine which profiles to show to you, and who can view your profile.
* A link to your Facebook profile, so users can see more of your content on Facebook (though the app has no access to that content directly),
* Your email address, to send match emails.

If you log in with Bluesky, the app uses data from your Bluesky account as follows:
* Your handle, name, and profile picture, to show to other users of the app according to your visibility settings,
* Your DID (Decentralized Identifier), to identify your account,
* The list of your mutuals (people who you follow who also follow you) who use the app, to determine which profiles to show to you, and who can view your profile.

Moreover, certain technical information about your connection to the app is
automatically gathered: User IP addresses, browser versions, and site resource
access patterns are stored temporarily for the purpose of monitoring and
debugging the technical functionality of the app or the system. They are not
accessed automatically as part of app functionality or used for any other
purpose.

All email notifications (when implemented) will be opt-in. (I may change the
privacy policy to allow them to default on if people want that, but this will
only take effect if you agree to the new policy.)

In general, the app keeps the data mentioned in this policy even when you don't
strictly use the functionality that needs it (e.g. your friend list when your
visibility is set to everyone). This is purely for simplicity of implementation
and reflects no intention to use the data for any other purpose.

This data is stored on and transmitted to e.g. hosting and service providers as
necessary for the operation of the app. Any third party who handles this data
will be subject to all the restrictions and obligations of this policy.

I may gather anonymised summary statistics of e.g. how many people use the app,
or how many people have how many matches, as part of ensuring the app is running
correctly, doing maintenance and debugging, developing additional features or
removing unused features, and e.g. reassuring AWS support that I'm not going to
send too many e-mails. For the avoidance of doubt, I will not intentionally look
at the private data of any individual user without their permission.

If a mistake in my code reveals to me information that I did not intend to see,
e.g. if a software component raises an error message that inadvertently contains
user data, I will take reasonable efforts to avoid reading the information, to
fix the system to avoid revealing the information in future, to contact you to
inform you of what I saw, and to update the website and privacy policy to
describe what kind of disclosure occurred (at time of writing, none have
happened so far). I will try to develop and deploy the service in such a way
that this doesn't happen, but this is a hobbyist project and not capable of
delivering complete assurance on this. You agree not to submit any data for
which this level of best-effort protection or the remedies I outlined are
inappropriate, and agree there will be no further consequences for an accidental
breach for which I follow the above procedure.

You can delete all the data held about you on your [account](/account) page.

If this policy is changed, you will continue to be subject to the terms of the
old policy until you explicitly agree to the new one. If I need to stop
supporting an older privacy policy, I will contact users if possible to ask them
to switch, and if I can't contact them, I will delete their accounts. I will aim
to leave at least 6 months since a policy was obsoleted before I delete accounts
relating to it, but if necessary I can delete any account at any time.
"""

view : List (Html msg)
view =
  case
    markdown
    |> Markdown.Parser.parse
    |> Result.mapError (String.join "\n" << List.map Markdown.Parser.deadEndToString)
    |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
  of
    Ok rendered -> rendered
    Err errors -> [ Html.pre [] [ Html.text markdown ] ]
