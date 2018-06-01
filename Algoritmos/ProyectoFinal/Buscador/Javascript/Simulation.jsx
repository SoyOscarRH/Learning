// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||           SIMULATION PAGE       ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
import React from "react"
import {HotKeys} from "react-hotkeys"


// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||           SIMULATION PAGE       ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
export default class Simulation extends React.Component {

    // =========================================================
    // ========        CONSTRUCTOR AND STATE         ===========
    // =========================================================
    constructor(props) {
        super(props)

        this.state = {
            value: null,
        }
    }


    // =========================================================
    // ============           RENDER              ==============
    // =========================================================
    render () {

        

        function ContentOfTable(props) {
            if (!!props.String1 && !!props.String2)
                return <GridTable String1={props.String1} String2={props.String2} />
            
            return <br />
        }

        return (
            <div className="card-panel blue-grey lighten-5 black-text">
   
                {/*=====================================================*/}
                {/*=========    SELECTOR FOR NEW PRODUCT    ============*/}
                {/*=====================================================*/}
                <div className="divider" />
                <div className="section">
                    <ContentOfTable String1={this.props.String1} String2={this.props.String2} />
                </div>
            </div>
        )
    }
}




function GridTable(props) {
    const String1Array = props.String1.split('')
    const String2Array = props.String2.split('')

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
                    {CellKey}
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