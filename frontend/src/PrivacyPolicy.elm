module PrivacyPolicy exposing (viewPrivacyPolicy)

import Html exposing (Html)
import Html.Attributes as Attributes

viewPrivacyPolicy : List (Html msg)
viewPrivacyPolicy =
  [ Html.h2 []
      [ Html.text "Privacy policy" ]
  , Html.p []
      [ Html.text """Below are detailed the kinds of data collected for the app,
          and the purposes for which it is collected. The purposes are intended
          to be exhaustive: no data is collected or can be used for any other
          purpose without your express prior consent."""
      ]
  , Html.p []
      [ Html.text """To avoid changing the privacy policy too often, I may list
          data collected for a purpose even if that purpose is not implemented
          yet or that data is not collected yet. Agreeing to the privacy policy
          nevertheless grants me permission to collect and use the information
          listed for the purposes listed in future."""
      ]
  , Html.p []
      -- https://developers.facebook.com/terms/#privacypolicy
      [ Html.text "The app uses data provided directly by you:"
      , [ "Your bio, to display to other users according to your visibility settings,"
        , """Your preferences and settings, in order to customise your experience
            of the app."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      , Html.text "If you log in with Google, the app receives and stores from them:"
      , [ """Your email address, which is used as your account identifier and to
            send match emails,"""
        , """Your name and profile picture, to show to other users of the app
            according to your visibility settings."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      , Html.text """If you log in with Facebook, the app uses data from
          Facebook as follows:"""
      , [ """Your name and profile picture, to show to other users of the app
            according to your visibility settings,"""
        , """The list of your friends who use the app, to determine which
            profiles to show to you, and who can view your profile."""
        , """A link to your Facebook profile, so users can see more of your
            content on Facebook (though the app has no access to that content
            directly),"""
        , "Your email address."
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      , Html.text """Moreover, certain technical information about your
          connection to the app is automatically gathered:"""
      , [ """User IP addresses, browser versions, and site resource access patterns
            are stored temporarily for the purpose of monitoring and debugging
            the technical functionality of the app or the system. They are not
            accessed automatically as part of app functionality or used for any
            other purpose."""
        ]
        |> List.map (Html.li [] << List.singleton << Html.text)
        |> Html.ul []
      ]
  , Html.p [] [
      Html.text """All email notifications (when implemented) will be opt-in.
        (I may change the privacy policy to allow them to default on if people
        want that, but this will only take effect if you agree to the new
        policy.)"""
    ]
  , Html.p [] [
      Html.text """In general, the app keeps the data mentioned in this policy
        even when you don't strictly use the functionality that needs it (e.g.
        your friend list when your visibility is set to everyone). This is
        purely for simplicity of implementation and reflects no intention to use
        the data for any other purpose."""
    ]
  , Html.p [] [
      Html.text """This data is stored on and transmitted to e.g. hosting and
        service providers as necessary for the operation of the app. Any third
        party who handles this data will be subject to all the restrictions and
        obligations of this policy."""
    ]
  , Html.p [] [
      Html.text """I may gather anonymised summary statistics of e.g. how many
        people use the app, or how many people have how many matches, as part of
        ensuring the app is running correctly, doing maintenance and debugging,
        developing additional features or removing unused features, and e.g.
        reassuring AWS support that I'm not going to send too many e-mails. I
        will never intentionally look at private data relating to a specific
        user whose identity I know."""
    ]
  , Html.p [] [
      Html.text """If a mistake in my code reveals to me information that I
        did not intend to see, e.g. if a software component raises an error
        message that inadvertently contains user data, I will take reasonable
        efforts to avoid reading the information, to fix the system to avoid
        revealing the information in future, to contact you to inform you of
        what I saw, and to update the website and privacy policy to describe
        what kind of disclosure occurred (at time of writing, none have happened
        so far). I will try to develop and deploy the service in such a way that
        this doesn't happen, but this is a hobbyist project and not capable of
        delivering complete assurance on this. You agree not to submit any data
        for which this level of best-effort protection or the remedies I
        outlined are inappropriate, and not to pursue any other remedies from me
        in such an event."""
    ]
  , Html.p []
      [ Html.text "You can delete all the data held about you on your "
      , Html.a
          [ Attributes.href "/account" ]
          [ Html.text "account" ]
      , Html.text " page."
      ]
  , Html.p [] [
      Html.text """If this policy is changed, you will continue to be subject to
        the terms of the old policy until you explicitly agree to the new
        one."""
    ]
  ]
