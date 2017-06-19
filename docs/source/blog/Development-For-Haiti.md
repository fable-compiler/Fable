- tagline: How Fable and Fable-elmish helped Casque Noir, A non profit organization 

## Casque Noir: Raising awareness on Haïti social and environmental issues

For several years, Casque Noir, a Canadian non-profit organization, has been researching about urban mutations of Haïti Island's capital, Port-au-Prince's most famous deprived urban neighbourhood: Jalousie

![Jalousie](http://www.casquenoir.com/img/jalousie/j_1_1a.jpg) _Credit: Nathalie Claude_

One of the main problem in *Jalousie* is rubbish spreading everywhere, mostly thanks to plastic based items, one being water bags called *Alaska*. The Guardian made a [photo report](https://www.theguardian.com/cities/gallery/2015/sep/28/cleaning-up-haiti-safest-water-source-contaminating-in-pictures) about it a few years ago.

![street filled with rubbish](http://www.casquenoir.com/img/jalousie/j_1_1b.jpg) _Credit: Nathalie Claude_


The [Blok’ô Kalité Materyo project](http://www.casquenoir.com/#jalousie) is an initiative to recycle water bags through the creation of light concrete blocks built by mixing platic and concrete.

![Blok’ô Kalité Materyo](http://www.casquenoir.com/img/jalousie/j_3_1a.jpg) _Credit: Nathalie Claude_


## The Web site

In early 2017, after many trips to Haïti, where she gathered a lot of material, [Nathalie Claude](https://www.linkedin.com/in/nathalie-claude-694a2045/), founder of [Casque Noir](https://www.facebook.com/casquenoir2013/) asked [Atelier BienSür](http://www.biensuratelier.com/graphisme.html) if it would be possible to make a web documentary to spread the word about Casque Noir's *Blok’ô Kalité Materyo* project.

![The web site](https://img15.hostingpics.net/pics/19445179cn.png)

The main constraint being time: nothing, neither texts nor videos,  sounds or photos would be ready before May. And the deadline was set to June the 10th with a public showing the 16th in Montreal.

Being a long time partner of [Atelier BienSür](http://www.biensuratelier.com/graphisme.html), Elsa Miquel contacted me to see what could be done technically in less than a month.

So with a very early mockup we gathered a list of requirements:
- full responsive
- dynamic and responsive navigation bar
- full screen photo display
- photos slideshows
- videos embeds
- sounds embeds

## The Tech stack

![Fable logo](https://user-images.githubusercontent.com/1197905/27047311-56453e74-4fb0-11e7-905c-0a05e5fd87ca.png)

Once again, I decided I would go with [Fable](http://fable.io/). And I asked my friend [Maxime Mangel](https://twitter.com/MangelMaxime) what he thought about using [Fable-Elmish](https://fable-elmish.github.io/elmish/) to build the site. He validated the combo fable-elmish with [bulma css framework](http://bulma.io/)

## The Architecture

_Please have a look at the [Fable-Elmish](https://fable-elmish.github.io/elmish/) documentation to understand what follows._

I ended up with very few Pages:

```f#
type Page =
  | Home
  | About
  | Jalousie
  | Montreal
  | WelcomePage
```

The documentary part is split into 2 sections which are :
1. [Jalousie](http://www.casquenoir.com/#jalousie)
2. [Montreal](http://www.casquenoir.com/#montreal)


Since each section uses the same layout I decided I would create reusable components such as video embeds:

```f#
  let youtube id =
    let realUrl = sprintf "https://www.youtube.com/embed/%s?controls=1&showinfo=0&rel=0&autoplay=0&loop=1&playlist=%s&rel=0&modestbranding=1" id id
    div [ClassName "columns" ]
      [
        div [ClassName "column is-half is-offset-2" ]
          [
            div [ ClassName "videowrapper"]
              [
              iframe
                [
                  Src realUrl
                  unbox("frameBorder","0")
                  unbox("allowFullScreen","allowFullScreen")
                ]
                [ ]
            ]
          ]
      ]
```

or [Font awesome icons](http://fontawesome.io/icons/)

```f#
  let icon name css =
    span
      [
        ClassName <| sprintf "icon %s" css
        unbox("aria-hidden", "true")
      ] [ i [ ClassName (sprintf "fa %s" name) ] [] ]
```

For the main layout I ended up with a very simple scheme:

```f#
  div [
  ][
    Navbar.View.root model.navbar (NavMsg >> dispatch ) model.currentPage //  navigation bar

    pageHtml model.currentPage // current page

    Credits.View.root model.credits (CreditsMsg >> dispatch ) // credits modal

    Intro.View.root model.intro (IntroMsg >> dispatch ) // intro modal

    Partners.View.root model.partners (PartnersMsg >> dispatch ) // partners modal
  ]
```

The *Credits*, *Intro* and *Partners* section are just [modal windows](http://bulma.io/documentation/components/modal/) that appear when needed from any section, so I added them in there.

The dynamic navigation bar is sticky so I added it there too.

```css
// Sticky navbar
.nav
  position: fixed !important
  top: 0
  left: 0
  right: 0
```

## The nav bar

![nav bar](https://img15.hostingpics.net/pics/904108navbar.gif)

This is a very important component in the application since it allows user to switch to any section/chapter as well as to subchapters.

The links to modal popups works like this:

```
    a [
      ClassName "nav-item"
      OnClick (fun _ ->
        DisplayCredits true |> dispatch
        HideBurger |> dispatch
        )
      Title title
    ] [ str title ]
```

The ```DisplayCredits``` message simply toggles the display of the modal window through the use of ```is-active``` css class as specified in [Bulma doc](http://bulma.io/documentation/components/modal/)

The ```HideBurger``` message hides the burger when the navbar is displayed on mobile phones. Again through the use of ```is-active``` css class as specified in [Bulma doc](http://bulma.io/documentation/components/nav/)


The most interesting part being the subsections. Clicking on a chapter number scrolls down the page to the section. So I created *scrollableTo* component:


```f#
let scrollableTo css tag elems dispatch=
  let id = (sprintf "nav%s" tag)
  a [
    Id id
    Href id
    ClassName css
    OnClick (fun _ ->

      document.getElementById( tag ).scrollIntoView()

      ActivateSection id |> dispatch

      HideBurger |> dispatch
      )
  ] [ elems ]
```

The code is pretty straightforward. We ask for the content through the ```scrollIntoView``` method. And we send an ```ActivateSection``` message which just updates the model with current section id and reloads the view to activate the relevant css class (colored background)

![nav bar](https://img15.hostingpics.net/pics/904108navbar.gif)


Regarding the sounds. Each section plays a sound when loaded. The speaker icon on the navbar mutes the sound through a ```ToggleSound``` message. I created a simple sound component.

*View*:
```f#
let sound model dispatch =

  let icon =
    match model.toggleSound with
    | true ->
      Components.icon "fa-volume-up" ""
    | false ->
      Components.icon "fa-volume-off" ""

  div [
    ClassName "nav-item nav-title"
    OnClick ( fun _ -> ToggleSound |> dispatch )
  ] [
    icon
  ]
```

*State*:
```f#
  match msg with
  | ToggleSound  ->
    let value = not model.toggleSound
    match value with
    | true -> Waud.setVolume 1.0
    | false -> Waud.setVolume 0.0

    { model with toggleSound = value}, []
```

## Straightforward development

The only external library I ended up using was [Waud.js](http://www.waudjs.com/) for the sound which offers many cool features, among others **automute** when we quit the web site to another tab or use another app.

All the other components were created for the app.

## Conclusion

![Fable... What else ?](https://i.imgflip.com/1qzmbn.jpg)

As a freelancer I don't have much time for
- debugging runtime errors
- costly maintenance after release

Usually my clients want more features for less money and of course _stability_

So in the search of a path for rapid and robust development, once again, Fable and Fable-Elmish just allowed me to do the job in due time.

Remember me: *If it compiles = it works!*
