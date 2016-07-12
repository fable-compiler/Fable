namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

module DC =
    type ILegendwidget =
        abstract x: float->float
        abstract x: unit->float
        abstract y: float->float
        abstract y: unit->float
        abstract gap: float->float
        abstract gap: unit->float
        abstract itemHeight: float->float
        abstract itemHeight: unit->float
        abstract horizontal: bool->bool
        abstract horizontal: unit->bool
        abstract legendWidth: float->float
        abstract legendWidth: unit->float
        abstract itemWidth: float->float
        abstract itemWidth: unit->float

    and IBaseChart<'T> =
        abstract width: float->'T
        abstract width: unit->'T
        abstract height: float->'T
        abstract height: unit->'T
        abstract minWidth: float->'T
        abstract minWidth: unit->'T
        abstract minHeight: float->'T
        abstract minHeight: unit->'T
        abstract dimension: obj->'T
        abstract dimension: unit->'T
        abstract group: obj->'T
        abstract group: unit->'T
        abstract transitionDuration: float->'T
        abstract transitionDuration: unit->'T
        abstract colors: string[]->'T
        abstract colors: unit->'T
        abstract keyAccessor: Func<obj, float>->'T
        abstract keyAccessor: unit->Func<obj, float>
        abstract valueAccessor: Func<obj, float>->'T
        abstract valueAccessor: unit->Func<obj, float>
        abstract label: Func<obj, string>->'T
        abstract label: unit->Func<obj, string>
        abstract renderLabel: bool->'T
        abstract renderLabel: unit->bool
        abstract renderlet: Func<'T, unit>->'T
        abstract title: Func<string, string>->'T
        abstract title: unit->Func<string, string>
        abstract filter: obj->'T
        abstract filter: unit->obj
        abstract filterAll: unit->unit
        abstract expireCache: unit->unit
        abstract legend: ILegendwidget->'T
        abstract chartID: unit->float
        abstract options: obj->unit
        abstract select: obj->obj
        abstract selectAll: obj->obj

    and IEvents =
        abstract trigger: fnctn: Func<unit> * ?delay: float -> unit

    and IListener<'T> =
        abstract on: string * Func<'T, unit> -> 'T

    and ImarginObj =
        abstract top: float with get, set
        abstract right: float with get, set
        abstract bottom: float with get, set
        abstract left: float with get, set

    and IMarginable<'T> =
        abstract margins: ImarginObj->'T
        abstract margins: unit->ImarginObj

    and IAbstractColorChart<'T> =
        abstract colorDomain: float[]->'T
        abstract colorDomain: unit->float[]

    and IAbstractStackableChart<'T> =
        abstract stack: IChartGroup * string * Func<obj, float> * 'T

    and IAbstractCoordinateGridChart<'T> =
        abstract x: obj->'T
        abstract x: unit->obj
        abstract y: obj->'T
        abstract y: unit->obj
        abstract elasticY: unit->bool
        abstract elasticY: bool->'T
        abstract xAxis: unit->obj
        abstract xAxis: obj->'T
        abstract yAxis: unit->obj
        abstract yAxis: obj->'T
        abstract yAxisPadding: unit->float
        abstract yAxisPadding: float->'T
        abstract xAxisPadding: unit->float
        abstract xAxisPadding: float->'T
        abstract renderHorizontalGridLines: unit->bool
        abstract renderHorizontalGridLines: bool->'T
    
    and IAbstractBubblechart<'T> =
        abstract r: obj->'T
        abstract r: unit->obj
        abstract radiusValueAccessor: Func<obj, float>->'T
        abstract radiusValueAccessor: unit->Func<obj, float>
    
    and IBubblechart =
        inherit IBaseChart<IBubblechart>
        inherit IAbstractColorChart<IBubblechart>
        inherit IAbstractBubblechart<IBubblechart>
        inherit IAbstractCoordinateGridChart<IBubblechart>
        inherit IMarginable<IBubblechart>
        inherit IListener<IBubblechart>

    and IPiechart =
        inherit IBaseChart<IPiechart>
        inherit IAbstractColorChart<IPiechart>
        inherit IAbstractBubblechart<IPiechart>
        inherit IAbstractCoordinateGridChart<IPiechart>
        inherit IMarginable<IPiechart>
        inherit IListener<IPiechart>
        abstract radius: float->IPiechart
        abstract radius: unit->float
        abstract minAngleForLabel: float->IPiechart
        abstract minAngleForLabel: unit->float

    and IBarchart =
        inherit IBaseChart<IBarchart>
        inherit IAbstractStackableChart<IBarchart>
        inherit IAbstractCoordinateGridChart<IBarchart>
        inherit IMarginable<IBarchart>
        inherit IListener<IBarchart>
        abstract centerBar: Func<bool, IBarchart> with get, set
        abstract gap: Func<float, IBarchart> with get, set

    and ILinechart =
        inherit IBaseChart<ILinechart>
        inherit IAbstractStackableChart<ILinechart>
        inherit IAbstractCoordinateGridChart<ILinechart>
        inherit IMarginable<ILinechart>
        inherit IListener<ILinechart>


    and IDatachart =
        inherit IBaseChart<IDatachart>
        inherit IAbstractStackableChart<IDatachart>
        inherit IAbstractCoordinateGridChart<IDatachart>
        inherit IMarginable<IDatachart>
        inherit IListener<IDatachart>
        abstract size: float->IDatachart
        abstract size: unit->float
        abstract columns: Func<obj,string>[]->IDatachart
        abstract columns: unit->Func<obj,string>[]
        abstract sortBy: Func<obj,obj>->IDatachart
        abstract sortBy: unit->Func<obj,obj>
        abstract order: Func<obj,obj,int>->IDatachart
        abstract order: unit->Func<obj,obj,int>

    and IRowchart =
        inherit IBaseChart<IRowchart>
        inherit IAbstractColorChart<IRowchart>
        inherit IAbstractStackableChart<IRowchart>
        inherit IAbstractCoordinateGridChart<IRowchart>
        inherit IMarginable<IRowchart>
        inherit IListener<IRowchart>


    and IChartGroup =
        interface end

    type [<Import("*","dc")>] Globals =
        static member events with get(): IEvents = failwith "JS only" and set(v: IEvents): unit = failwith "JS only"
        static member filterAll(?chartGroup: IChartGroup): unit = failwith "JS only"
        static member renderAll(?chartGroup: IChartGroup): unit = failwith "JS only"
        static member redrawAll(?chartGroup: IChartGroup): unit = failwith "JS only"
        static member bubbleChart(cssSel: string): IBubblechart = failwith "JS only"
        static member pieChart(cssSel: string): IPiechart = failwith "JS only"
        static member barChart(cssSel: string): IBarchart = failwith "JS only"
        static member lineChart(cssSel: string): ILinechart = failwith "JS only"
        static member dataTable(cssSel: string): IDatachart = failwith "JS only"
        static member rowChart(cssSel: string): IRowchart = failwith "JS only"

