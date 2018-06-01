import React from "react";
import {HotKeys} from "react-hotkeys"
import AppHeader from "./AppHeader"
import Simulation from "./Simulation"



// =====================================================================
// ============      APP COMPONENTS        =============================
// =====================================================================
export default class App extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
            String1: "Hola", 
            String2: "Bebe",
            NewString1: "Hola",
            NewString2: "Bebe",
            IsBarOpen: false,
            HotKeysMap: {
                keyMap: {
                    ShowBar:            ['shift+b', 'alt+b', 'ctrl+b'],
                    ChangeStrings:      ['shift+c',  'alt+c',  'ctrl+c'],
                },
                handlers: {
                    'ShowBar': (e) => {
                        const Element = document.getElementById('SideMenu')
                        const SideMenu = M.Sidenav.getInstance(Element);
                        (this.state.IsBarOpen)? SideMenu.close(): SideMenu.open()
                        this.setState({IsBarOpen: !this.state.IsBarOpen})
                    },
                    'ChangeStrings': (e) => {
                        M.Modal.getInstance(document.getElementById('SubmissionModal')).open()
                    }
                }
            }
        }

        document.addEventListener('DOMContentLoaded', function() {
            const OptionsModals = {dismissible: true, inDuration: 40, outDuration: 40}
            M.Modal.init(document.getElementById('ErrorModal'), OptionsModals)
            M.Modal.init(document.getElementById('SubmissionModal'), OptionsModals)
        })
    }

    handleStrings(Event, Index) {
        if (Index == 1) this.setState({String1: Event.target.value})
        else this.setState({String2: Event.target.value})
    }


    render () {

        const SendNewStrings = () => {

            if (this.state.String1 == "" || this.state.String2 == "") {
                M.toast({html: 'Cadenas no válidas'})
                return
            }

            if (this.state.String1.trim() == "" || this.state.String2.trim() == "") {
                M.toast({html: 'Cadenas no válidas'})
                return
            }

            this.setState({'NewString1': this.state.String1, 'NewString2': this.state.String2})
            M.Modal.getInstance(document.getElementById('SubmissionModal')).close()
        }

        return (
            <div>
            <HotKeys keyMap={this.state.HotKeysMap.keyMap} handlers={this.state.HotKeysMap.handlers}>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++           HEADERS              ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <header>
                    <AppHeader Name={"Subsecuencia cómun más larga"} />
                </header>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++         SIMULATION             ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <div className="container">
                    <div className="center-align row section">
                        <div className="col s12">
                            <Simulation String1={this.state.NewString1} String2={this.state.NewString2} />
                        </div>
                    </div>
                </div>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++         SUBMISSION MODAL       ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <HotKeys keyMap={{"Send": 'enter'}} handlers={{"Send": (e) => {SendNewStrings()}}}>
                    <div id="SubmissionModal" className="modal modal-fixed-footer" style={{width:'80%'}}>
                        <div className="modal-content">
                            <div className="container">
                                <h4 className="center-align blue-grey-text text-darken-3">
                                    <strong>Cadenas</strong> para Animación
                                </h4>
                                <br />
                                <div className="row">
                                    <div className="input-field col s12">
                                        <input 
                                            id       = "String1" 
                                            value    = {this.state.String1} 
                                            onChange = {e => this.handleStrings(e, 1)} 
                                            type     = "text"/>
                                        <label htmlFor="String1">Cadena 1</label>
                                    </div>
                                </div>
                                <div className="row">
                                    <div className="input-field col s12">
                                        <input 
                                            id       = "String2" 
                                            value    = {this.state.String2} 
                                            onChange = {e => this.handleStrings(e, 2)} 
                                            type     = "text"/>
                                        <label htmlFor="String2">Cadena 2</label>
                                    </div>
                                </div>
                                <div className="row">
                                    <div className="col s4 offset-s4">
                                        <a onClick={SendNewStrings} className="waves-effect waves-light green lighten-1 btn-large">
                                            Cambiar
                                        </a>
                                    </div>
                                </div>
                            </div>
                        </div>
                        <div className="modal-footer">
                            <a className="btn-flat modal-close waves-effect waves-red red lighten-2">
                                <span className="white-text">Cancelar</span>
                            </a>
                        </div>
                    </div>
                </HotKeys>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++          ERROR MODAL           ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <HotKeys 
                    keyMap   = {{"CloseModal": 'enter'}}
                    handlers = {{
                        "CloseModal": (e) => {
                            M.Modal.getInstance(document.getElementById('ErrorModal')).close()
                        }
                    }}>
                    <div 
                        id        = "ErrorModal"
                        className = "modal modal-fixed-footer"
                        style     = {{width: '70%'}} >
                        
                        <div className="modal-content">
                            <h4>Error</h4>
                            Error
                        </div>

                        <div className="modal-footer">
                            <a className="btn-flat modal-close waves-effect waves-red red lighten-2">
                                <span className="white-text">Salir</span>
                            </a>
                        </div>
                    </div>
                </HotKeys>

            </HotKeys>
            </div>
        );
    }
}


