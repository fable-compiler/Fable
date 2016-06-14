namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.React
module H = Fable.Import.HistoryModule

// Type definitions for react-router v2.0.0; generated from Typescript with some modifications.
module ReactRouter =
    type Component =
        React.ReactType

    and EnterHook =
        Func<RouterState, RedirectFunction, Function, unit>

    and LeaveHook =
        Func<unit>

    and ChangeHook =
        Func<RouterState, RouterState, RedirectFunction, Function, unit>

    and Params =
        obj

    and ParseQueryString =
        Func<H.QueryString, H.Query>

    and RedirectFunction =
        [<Emit("$0($1...)")>] abstract Invoke: location: H.LocationDescriptor -> unit
        [<Emit("$0($1...)")>] abstract Invoke: state: H.LocationState * pathname: U2<H.Pathname, H.Path> * ?query: H.Query -> unit

    and RouteComponent =
        Component

    and RouteComponentProps<'P, 'R> =
        abstract history: History option with get
        abstract location: H.Location option with get
        abstract ``params``: 'P option with get
        abstract route: PlainRoute option with get
        abstract routeParams: 'R option with get
        abstract routes: ResizeArray<PlainRoute> option with get
        abstract children: React.ReactElement<obj> option with get

    and RouteComponents =
        obj

    and RouteConfig =
        U3<React.ReactNode, PlainRoute, ResizeArray<PlainRoute>>

    and RouteHook =
        Func<H.Location, obj>

    and RoutePattern =
        string

    and StringifyQuery =
        Func<H.Query, H.QueryString>

    and RouterListener =
        Func<Error, RouterState, unit>

    and RouterState =
        abstract location: H.Location with get
        abstract routes: ResizeArray<PlainRoute> with get
        abstract ``params``: Params with get
        abstract components: ResizeArray<RouteComponent> with get

    and HistoryBase =
        inherit H.History
        abstract routes: ResizeArray<PlainRoute> with get
        abstract parseQueryString: ParseQueryString option with get
        abstract stringifyQuery: StringifyQuery option with get

    and History =
        obj

    and RouterProps =
        inherit React.Props<Router>
        abstract history: H.History option with get
        abstract routes: RouteConfig option with get
        abstract createElement: Func<RouteComponent, obj, obj> option with get
        abstract onError: Func<obj, obj> option with get
        abstract onUpdate: Func<obj> option with get
        abstract parseQueryString: ParseQueryString option with get
        abstract stringifyQuery: StringifyQuery option with get

    and Router =
        inherit React.ComponentClass<RouterProps>


    and RouterElement =
        inherit React.ReactElement<RouterProps>


    and LinkProps =
        inherit React.HTMLAttributes
        inherit React.Props<Link>
        abstract activeStyle: React.CSSProperties option with get
        abstract activeClassName: string option with get
        abstract onlyActiveOnIndex: bool option with get
        abstract ``to``: U2<RoutePattern, H.LocationDescriptor> with get
        abstract query: H.Query option with get
        abstract state: H.LocationState option with get

    and Link =
        inherit React.ComponentClass<LinkProps>


    and LinkElement =
        inherit React.ReactElement<LinkProps>


    and RouterContextProps =
        inherit React.Props<RouterContext>
        abstract history: H.History option with get
        abstract router: Router with get
        abstract createElement: Func<RouteComponent, obj, obj> with get
        abstract location: H.Location with get
        abstract routes: RouteConfig with get
        abstract ``params``: Params with get
        abstract components: ResizeArray<RouteComponent> option with get

    and RouterContext =
        inherit React.ComponentClass<RouterContextProps>


    and RouterContextElement =
        inherit React.ReactElement<RouterContextProps>
        abstract history: H.History option with get
        abstract location: H.Location with get
        abstract router: Router option with get

    and RouteProps =
        inherit React.Props<Route>
        abstract path: RoutePattern option with get
        abstract ``component``: RouteComponent option with get
        abstract components: RouteComponents option with get
        abstract getComponent: Func<H.Location, Func<obj, RouteComponent, unit>, unit> option with get
        abstract getComponents: Func<H.Location, Func<obj, RouteComponents, unit>, unit> option with get
        abstract onEnter: EnterHook option with get
        abstract onLeave: LeaveHook option with get
        abstract onChange: ChangeHook option with get
        abstract getIndexRoute: Func<H.Location, Func<obj, RouteConfig, unit>, unit> option with get
        abstract getChildRoutes: Func<H.Location, Func<obj, RouteConfig, unit>, unit> option with get

    and Route =
        inherit React.ComponentClass<RouteProps>


    and RouteElement =
        inherit React.ReactElement<RouteProps>


    and PlainRoute =
        abstract path: RoutePattern option with get
        abstract ``component``: RouteComponent option with get
        abstract components: RouteComponents option with get
        abstract getComponent: Func<H.Location, Func<obj, RouteComponent, unit>, unit> option with get
        abstract getComponents: Func<H.Location, Func<obj, RouteComponents, unit>, unit> option with get
        abstract onEnter: EnterHook option with get
        abstract onLeave: LeaveHook option with get
        abstract indexRoute: PlainRoute option with get
        abstract getIndexRoute: Func<H.Location, Func<obj, RouteConfig, unit>, unit> option with get
        abstract childRoutes: ResizeArray<PlainRoute> option with get
        abstract getChildRoutes: Func<H.Location, Func<obj, RouteConfig, unit>, unit> option with get

    and RedirectProps =
        inherit React.Props<Redirect>
        abstract path: RoutePattern option with get
        abstract from: RoutePattern option with get
        abstract ``to``: RoutePattern with get
        abstract query: H.Query option with get
        abstract state: H.LocationState option with get

    and Redirect =
        inherit React.ComponentClass<RedirectProps>


    and RedirectElement =
        inherit React.ReactElement<RedirectProps>


    and IndexRouteProps =
        inherit React.Props<IndexRoute>
        abstract ``component``: RouteComponent option with get
        abstract components: RouteComponents option with get
        abstract getComponent: Func<H.Location, Func<obj, RouteComponent, unit>, unit> option with get
        abstract getComponents: Func<H.Location, Func<obj, RouteComponents, unit>, unit> option with get
        abstract onEnter: EnterHook option with get
        abstract onLeave: LeaveHook option with get

    and IndexRoute =
        inherit React.ComponentClass<IndexRouteProps>


    and IndexRouteElement =
        inherit React.ReactElement<IndexRouteProps>


    and IndexRedirectProps =
        inherit React.Props<IndexRedirect>
        abstract ``to``: RoutePattern with get
        abstract query: H.Query option with get
        abstract state: H.LocationState option with get

    and IndexRedirect =
        inherit React.ComponentClass<IndexRedirectProps>


    and IndexRedirectElement =
        inherit React.ReactElement<IndexRedirectProps>


    and RouterOnContext =
        inherit H.History
        abstract setRouteLeaveHook: route: PlainRoute * ?hook: RouteHook -> Func<unit>
        abstract isActive: pathOrLoc: H.LocationDescriptor * ?indexOnly: bool -> bool

    and HistoryMixin =
        abstract history: History with get

    and LifecycleMixin =
        abstract routerWillLeave: nextLocation: H.Location -> U2<string, bool>

    and HistoryRoutes =
        abstract listen: listener: RouterListener -> Function
        abstract listenBeforeLeavingRoute: route: PlainRoute * hook: RouteHook -> unit
        abstract ``match``: location: H.Location * callback: Func<obj, RouterState, H.Location, unit> -> unit
        abstract isActive: pathname: H.Pathname * ?query: H.Query * ?indexOnly: bool -> bool
        abstract setRouteLeaveHook: route: PlainRoute * callback: RouteHook -> unit

    and MatchArgs =
        abstract routes: RouteConfig option with get
        abstract history: H.History option with get
        abstract location: U2<H.Location, string> option with get
        abstract parseQueryString: ParseQueryString option with get
        abstract stringifyQuery: StringifyQuery option with get
        abstract basename: string option with get

    and MatchState =
        inherit RouterState
        abstract history: History with get

    type [<Import("*","ReactRouter")>] Globals =
        static member browserHistory with get(): History = failwith "JS only" and set(v: History): unit = failwith "JS only"
        static member hashHistory with get(): History = failwith "JS only" and set(v: History): unit = failwith "JS only"
        static member Router with get(): Router = failwith "JS only" and set(v: Router): unit = failwith "JS only"
        static member Link with get(): Link = failwith "JS only" and set(v: Link): unit = failwith "JS only"
        static member IndexLink with get(): Link = failwith "JS only" and set(v: Link): unit = failwith "JS only"
        static member RouterContext with get(): RouterContext = failwith "JS only" and set(v: RouterContext): unit = failwith "JS only"
        static member Route with get(): Route = failwith "JS only" and set(v: Route): unit = failwith "JS only"
        static member Redirect with get(): Redirect = failwith "JS only" and set(v: Redirect): unit = failwith "JS only"
        static member IndexRoute with get(): IndexRoute = failwith "JS only" and set(v: IndexRoute): unit = failwith "JS only"
        static member IndexRedirect with get(): IndexRedirect = failwith "JS only" and set(v: IndexRedirect): unit = failwith "JS only"
        static member History with get(): React.Mixin<obj, obj> = failwith "JS only" and set(v: React.Mixin<obj, obj>): unit = failwith "JS only"
        static member Lifecycle with get(): React.Mixin<obj, obj> = failwith "JS only" and set(v: React.Mixin<obj, obj>): unit = failwith "JS only"
        static member RouteContext with get(): React.Mixin<obj, obj> = failwith "JS only" and set(v: React.Mixin<obj, obj>): unit = failwith "JS only"
        static member createMemoryHistory(?options: H.HistoryOptions): H.History = failwith "JS only"
        static member useRoutes(createHistory: HistoryModule.CreateHistory<'T>): HistoryModule.CreateHistory<obj> = failwith "JS only"
        static member createRoutes(routes: RouteConfig): ResizeArray<PlainRoute> = failwith "JS only"
        static member ``match``(args: MatchArgs, cb: Func<obj, H.Location, MatchState, unit>): unit = failwith "JS only"



module ReactRouterPatternUtils =
    type [<Import("*","react-router/lib/PatternUtils")>] Globals =
        static member formatPattern(pattern: string, ``params``: obj): string = failwith "JS only"



module ReactRouterRouteUtils =
    type E = ReactElement<obj>

    type [<Import("*","react-router/lib/RouteUtils")>] Globals =
        static member isReactChildren(``object``: U2<E, ResizeArray<E>>): bool = failwith "JS only"
        static member createRouteFromReactElement(element: E): ReactRouter.PlainRoute = failwith "JS only"
        static member createRoutesFromReactChildren(children: U2<E, ResizeArray<E>>, parentRoute: ReactRouter.PlainRoute): ResizeArray<ReactRouter.PlainRoute> = failwith "JS only"



module ReactRouterPropTypes =
    type [<Import("*","react-router/lib/PropTypes")>] Globals =
        static member history with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member location with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member ``component`` with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member components with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member route with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member routes with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member routerShape with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member locationShape with get(): React.Requireable<obj> = failwith "JS only" and set(v: React.Requireable<obj>): unit = failwith "JS only"
        static member falsy(props: obj, propName: string, componentName: string): Error = failwith "JS only"



module ReactRouteruseRouterHistory =
    type CreateRouterHistory =
        [<Emit("$0($1...)")>] abstract Invoke: ?options: HistoryModule.HistoryOptions -> obj

    type [<Import("*","react-router/lib/useRouterHistory")>] Globals =
        static member useRouterHistory(createHistory: HistoryModule.CreateHistory<'T>): CreateRouterHistory = failwith "JS only"
