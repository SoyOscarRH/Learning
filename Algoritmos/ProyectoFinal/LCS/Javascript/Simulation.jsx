// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||           SIMULATION PAGE       ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
import React from "react"
import {HotKeys} from "react-hotkeys"
import './OverFlowButHide.css'

// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||           SIMULATION PAGE       ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
export default class Simulation extends React.Component {

    // =================================================
    // ========        CONSTRUCTOR AND STATE     =======
    // =================================================
    constructor(props) {
        super(props)

        const A = this.props.String1
        const B = this.props.String2

        let DPTable = Array(A.length + 1)
        for (let i = 0; i <= A.length; ++i) {   
            DPTable[i] = Array(B.length + 1)                        
            for (let j = 0; j <= B.length; ++j) { 
                DPTable[i][j] = 
                    (i === 0 || j === 0)? 0:
                        (A[i - 1] === B[j - 1])? DPTable[i-1][j-1] + 1:                  
                            Math.max(DPTable[i-1][j], DPTable[i][j-1])
            }
        }

        this.state = {
            VerticalIndex: 0,
            HorizontalIndex: 0,
            String1: this.props.String1,
            String2: this.props.String2,
            Table: DPTable
        }
    }

    // =================================================
    // ========        CONSTRUCTOR AND STATE     =======
    // =================================================
    static getDerivedStateFromProps(nextProps, PrevState) {
        
        if (nextProps.String1 === PrevState.String1 && nextProps.String2 === PrevState.String2) 
            return null

        const A = nextProps.String1
        const B = nextProps.String2

        let DPTable = Array(A.length + 1)
        for (let i = 0; i <= A.length; ++i) {   
            DPTable[i] = Array(B.length + 1)                        
            for (let j = 0; j <= B.length; ++j) { 
                DPTable[i][j] = 
                    (i === 0 || j === 0)? 0:
                        (A[i - 1] === B[j - 1])? DPTable[i-1][j-1] + 1:                  
                            Math.max(DPTable[i-1][j], DPTable[i][j-1])
            }
        }


        return {
            VerticalIndex: 0,
            HorizontalIndex: 0,
            String1: nextProps.String1,
            String2: nextProps.String2,
            Table: DPTable
        }

    }


    // =================================================
    // ========       DP TABLE AS ITSELF         =======
    // =================================================
    CreateTableRepresentation () {

        const Table = this.state.Table

        const NewString1 = this.props.String1 + ' '
        const NewString2 = this.props.String2 + ' '

        return NewString1.split('').map( (HorizontalChar, HorizontalIndex) => {
            return NewString2.split('').map( (VerticalChar, VerticalIndex)=> {

                const SemiString1 = this.props.String1.slice(0, HorizontalIndex) 
                const SemiString2 = this.props.String2.slice(0, VerticalIndex) 
                const value = 
                    (HorizontalIndex === 0 || VerticalIndex === 0)?
                    'Recuerda que la primera fila y columna se inician con 0'
                    :`¿Cuál es la subsecuencia más larga entre ${SemiString1} y ${SemiString2}?`

                let CardClass = "div z-depth-1 tooltipped "
                if (HorizontalIndex === this.props.String1.length && VerticalIndex === this.props.String2.length
                    && HorizontalIndex + 1 === this.state.HorizontalIndex && VerticalIndex + 1 === this.state.VerticalIndex)
                    CardClass += "red lighten-3"
                else if (HorizontalIndex === this.state.HorizontalIndex && VerticalIndex === this.state.VerticalIndex)
                    CardClass += "green accent-1"
                else if (this.props.String1[HorizontalIndex - 1] === this.props.String2[VerticalIndex-1])
                    CardClass += "cyan lighten-4"
                else
                    CardClass += "cyan lighten-5"

                let CardContent = " "
                if (VerticalIndex === 0 || HorizontalIndex === 0) CardContent = Table[HorizontalIndex][VerticalIndex]
                if (VerticalIndex < this.state.VerticalIndex) CardContent = Table[HorizontalIndex][VerticalIndex]
                if(VerticalIndex === this.state.VerticalIndex && HorizontalIndex <= this.state.HorizontalIndex)
                    CardContent = Table[HorizontalIndex][VerticalIndex]

                
                const StyleMiniCard = {
                    width: '50%',
                    height: '50%',
                    marginLeft: 'auto',
                    marginRight: 'auto'
                }
                

                return (
                    <div
                        className     = {CardClass}
                        data-position = "top"
                        style = {StyleMiniCard}
                        data-tooltip  = {value}>
                        <span className="blue-grey-text text-darken-3">
                            {CardContent}
                        </span>
                    </div>
                )
            })
        })
    }

    // =================================================
    // ========          NEXT STEP               =======
    // =================================================
    NextStep() {
        const Instantes = document.querySelectorAll('.tooltipped')
        Instantes.forEach((Instante) => {
            M.Tooltip.getInstance(Instante).destroy()
        })

        this.setState((PrevState) => {

            if (PrevState.VerticalIndex === 0 || PrevState.HorizontalIndex === 0) {
                return {VerticalIndex: 1, HorizontalIndex: 1}
            }
            else {
                let VerticalIndex = PrevState.VerticalIndex
                let HorizontalIndex = PrevState.HorizontalIndex + 1
                if (HorizontalIndex > this.props.String1.length) {
                    HorizontalIndex = 1
                    VerticalIndex = PrevState.VerticalIndex + 1

                    if (VerticalIndex > this.props.String2.length) {
                        HorizontalIndex = this.props.String1.length + 1
                    }

                    if (VerticalIndex > this.props.String2.length + 1) {
                        VerticalIndex -= 1
                        HorizontalIndex = this.props.String1.length + 1
                    }
                }

                return {VerticalIndex: VerticalIndex, HorizontalIndex: HorizontalIndex}
            }
        })
    }

    // =================================================
    // ========          PREV STEP               =======
    // =================================================
    PreviousStep() {

        const Instantes = document.querySelectorAll('.tooltipped')
        Instantes.forEach((Instante) => {
            M.Tooltip.getInstance(Instante).destroy()
        })

        this.setState((PrevState) => {
            
            if (PrevState.VerticalIndex === 0 && PrevState.HorizontalIndex === 0) {
                return {VerticalIndex: 0, HorizontalIndex: 0}
            }
            else if (PrevState.VerticalIndex === this.props.String2.length + 1 && PrevState.HorizontalIndex === this.props.String1.length + 1) {
                return {VerticalIndex: PrevState.VerticalIndex - 1, HorizontalIndex: PrevState.HorizontalIndex - 1}
            }
            else if (PrevState.VerticalIndex === 1 && PrevState.HorizontalIndex === 1) {
                return {VerticalIndex: 0, HorizontalIndex: 0}
            }
            else {
                let VerticalIndex = PrevState.VerticalIndex
                let HorizontalIndex = PrevState.HorizontalIndex - 1
                if (HorizontalIndex == 0) {
                    HorizontalIndex = this.props.String1.length
                    VerticalIndex = PrevState.VerticalIndex - 1
                }

                return {VerticalIndex: VerticalIndex, HorizontalIndex: HorizontalIndex}
            }
        })
    }

    // =================================================
    // ========          RESET STEP              =======
    // =================================================
    ResetStep() {
        const Instantes = document.querySelectorAll('.tooltipped')
        Instantes.forEach((Instante) => {
            M.Tooltip.getInstance(Instante).destroy()
        })

        M.toast({html: 'Regresando al principio del algoritmo', displayLength: 2000})
        this.setState({VerticalIndex: 0, HorizontalIndex: 0})
    }


    // =========================================================
    // ============           RENDER              ==============
    // =========================================================
    render () {

        function ContentOfTable (props) {
            if (!!props.String1 && !!props.String2)
                return <GridTable 
                            String1 = {props.String1} 
                            String2 = {props.String2}
                            VerticalIndex  = {props.VerticalIndex}
                            HorizontalIndex  = {props.HorizontalIndex}
                            Cells   = {props.Cells}
                        />
            return <br />
        }


        let StepText = ""

        if (this.state.VerticalIndex === 0 || this.state.HorizontalIndex === 0) 
            StepText = `LLenamos la primera fila y columna con puros 0. Despúes de todo la subsecuencia común más larga de dos cadenas
                        donde una de ellas es "" es 0`
        else if (this.state.HorizontalIndex === this.props.String1.length + 1 && this.state.VerticalIndex === this.props.String2.length + 1) {
            StepText = `Ya acabamos, nuestra respuesta esta en la tabla, en la posición ${this.state.HorizontalIndex - 1}, ${this.state.VerticalIndex - 1}`
            M.toast({html: 'Algoritmo Terminado', displayLength: 3000})
        }
        else if (this.props.String1[this.state.HorizontalIndex - 1] === this.props.String2[this.state.VerticalIndex-1]) 
            StepText = `Encontramos que "${this.props.String1}" en la posición ${this.state.HorizontalIndex} SI es igual a "${this.props.String2}"
                        en la posición ${this.state.VerticalIndex} por lo tanto la subsecuencia más larga actualmente es ahora
                        mayor por 1 a la que subsecuencia común más grande entre los strings "${this.props.String1.slice(0, this.state.HorizontalIndex - 1)}"
                        y "${this.props.String2.slice(0, this.state.VerticalIndex - 1)}"`
        else 
            StepText = `Encontramos que "${this.props.String1}" en la posición ${this.state.HorizontalIndex} NO es igual a "${this.props.String2}"
                        en la posición ${this.state.VerticalIndex} por lo tanto la subsecuencia más larga hasta el momento
                        es la mayor de la que podemos formar ya sea entre los strings "${this.props.String1.slice(0, this.state.HorizontalIndex - 1)}"
                        y "${this.props.String2.slice(0, this.state.VerticalIndex)}" o los strings 
                        "${this.props.String1.slice(0, this.state.HorizontalIndex)}" y "${this.props.String2.slice(0, this.state.VerticalIndex - 1)}"`

        return (
            <HotKeys 
                keyMap   = {{PrevStep: 'left', NextStep: 'right', Reset: 'r'}}
                handlers = {{
                    "PrevStep": (e) => this.PreviousStep(),
                    "NextStep": (e) => this.NextStep(),
                    "Reset":    (e) => this.ResetStep(),
                }}
            >
                {/*=====================================================*/}
                {/*=========    SELECTOR FOR NEW PRODUCT    ============*/}
                {/*=====================================================*/}
                <div className="card-panel grey lighten-5">
                    <h4 className="center-align blue-grey-text text-darken-2">
                        <strong>Algoritmo</strong> Paso a Paso
                    </h4>
                    <br />

                    <div className="row">
                        <div className="col s10 offset-s1 blue-grey-text darken-2-text">
                            {StepText}
                        </div>
                    </div>

                    <div className="row">
                        <div className="col s10 offset-s1">
                            {this.state.HorizontalIndex === 0 && this.state.VerticalIndex === 0 &&
                                <a onClick={(e) => this.NextStep()} className="waves-effect waves-light btn-large">
                                    Empezar Solución
                                </a>
                            }
                        </div>
                    </div>

                    <br />
                    <br />

                    <div className="row">
                        <div className="col s4">
                            <button 
                                onClick = {(e) => this.PreviousStep()}
                                className = "waves-effect btn-floating waves-light green btn-large btn-flat">
                                <i className="material-icons">arrow_back</i>
                            </button>
                        </div>
                        <div className="col s4">
                            <button 
                                onClick = {(e) => this.ResetStep()}
                                className = "waves-effect btn-floating waves-light green btn-large btn-flat">
                                <i className="material-icons">cached</i>
                            </button>
                        </div>
                        <div className="col s4">
                            <button 
                                onClick = {(e) => this.NextStep()}
                                className = "waves-effect btn-floating waves-light green btn-large btn-flat">
                                <i className="material-icons">arrow_forward</i>
                            </button>
                        </div>
                    </div>

                </div>

                {/*=====================================================*/}
                {/*=========        TABLE FOR A CARD        ============*/}
                {/*=====================================================*/}
                <div id="CardTable" style={{'overflowX': 'scroll'}} className="card-panel z-depth-2">
                    <h4 className="center-align blue-grey-text text-darken-2">
                        <strong>Tabla</strong> de Programación Dinámica
                    </h4>
                    <ContentOfTable 
                        String1 = {this.props.String1} 
                        String2 = {this.props.String2}
                        Cells   = {this.CreateTableRepresentation()}
                    />
                </div>
            </HotKeys>
        )
    }
}




// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||           SIMULATION PAGE       ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
class GridTable extends React.Component {

    constructor(props) {super(props)}

    componentDidMount() {
        const Elements = document.querySelectorAll('.tooltipped')
        M.Tooltip.init(Elements, {enterDelay: 250})
    }

    render () {

        const String1Array = this.props.String1.split('')
        const String2Array = this.props.String2.split('')

        String1Array.unshift(" ")
        String2Array.unshift(" ")
        
        const Word1Header = String1Array.map( (Letter, HorizontalIndex) => {
            const KeyHeader = `HeaderHorizontal${HorizontalIndex}`
            return (
                <td className="center-align " key={KeyHeader}>
                    <h5 className="green-text text-darken-1">
                        <strong>{Letter}</strong>
                    </h5>
                </td>
            )
        })
        
        const TableCells = String2Array.map((Letter, VerticalIndex) => {

            const KeyHeader = `Row${VerticalIndex}`
            const NormalRows = String1Array.map((Letter, HorizontalIndex) => {
                const CellKey = `${HorizontalIndex} ${VerticalIndex}`
                return (
                    <td className="center-align" key={CellKey}>
                        {this.props.Cells[HorizontalIndex][VerticalIndex]}
                    </td>
                )
            })

            const CellKey = `-1${VerticalIndex}`
            return (
                <tr key={KeyHeader} style={{border: 'none'}}>
                    <td className="center-align" key={CellKey}>
                        <h5 className="pink-text text-lighten-1">
                            <strong>{Letter}</strong>
                        </h5>
                    </td>
                    {NormalRows}
                </tr>
            )
        })

        return (
            <table className="browser-default">
                <tbody>
                    <tr style={{border: 'none'}}>
                        <td></td>
                        {Word1Header}
                    </tr>
                    {TableCells}
                </tbody>
            </table>
        )
    }

}