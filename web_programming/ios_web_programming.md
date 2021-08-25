# iOS web programming

## Web Inspector

### On Mac
Safari -> Preferences -> Advanced -> Show Develop in menu bar

### On iPhone
Settings -> Safari -> Advanced -> Web Inspector

## Application Name

	<title>WebApp</title>


## Launcher icon (iOS 1.1.3+)

Rounded corners, no added shiny (iOS 4.2):

	<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png"/>

Added shiny:
	<link rel="apple-touch-icon" href="touch-icon-iphone.png" />
	<link rel="apple-touch-icon" sizes="72x72" href="touch-icon-ipad.png" />
	<link rel="apple-touch-icon" sizes="114x114" href="touch-icon-iphone4.png" />

With no sizes the default is 57x57 px
(Multiple sizes iOS 4.2+)

## Startup Image (iOS 3.0+)

	<link rel="apple-touch-startup-image" href="/startup.png">

320x460 px, portrait

## Have standalone look

	<meta name="apple-mobile-web-app-capable" content="yes" />

``You might think it’s an innocent meta tag, but in fact it’s a powerful and dangerous meta tag if you add it irresponsibly. You must provide a single page application solution offering back buttons inside the UI -or use location.href instead of normal <a> links if you don’t want them to be opened in the browser instead of your app’s container-. ``<https://medium.com/@firt/dont-use-ios-web-app-meta-tag-irresponsibly-in-your-progressive-web-apps-85d70f4438cb>

``That will involve: a) adding back navigation everywhere, b) create a SPA experience or use location.href instead of <a> links for internal navigation, c) if the load process is done in fullscreen mode (navigator.standalone==true), always load the home screen not matter what the stored URL says and please d) don’t suggest me to “download an app” if I’m already inside an app-like experience.``

## Hide top status bar

NB - must have standalone mode on.

	<meta name="apple-mobile-web-app-status-bar-style" content="black" />


## Prevent zooming

	<meta name="viewport" content="initial-scale=1.0">
	<meta name="viewport" content="maximum-scale=1.0">
	<meta name="viewport" content="user-scalable=no">
	<meta name="viewport" content="width=device-width">


## Links

<https://developer.apple.com/library/archive/documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html>

