import React from "react";
import {HotKeys} from "react-hotkeys"
import AppHeader from "./AppHeader"
import Simulation from "./Simulation"



// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
// ||||||||||||            APP GENERAL          ||||||||||||||||||||||||
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
export default class App extends React.Component {


    // =================================================
    // ========         CONSTRUCTOR              =======
    // =================================================
    constructor(props) {
        super(props)
        this.state = {
            String1: "BDCAB", 
            String2: "ABCB",
            NewString1: "BDCAB",
            NewString2: "ABCB",
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
            M.Modal.init(document.getElementById('TutorialModal'), OptionsModals)
            M.Modal.init(document.getElementById('SubmissionModal'), OptionsModals)
            document.getElementById('CardTable').focus();
        })
    }

    // =================================================
    // ========         HANDLE STRINGS           =======
    // =================================================
    handleStrings(Event, Index) {
        if (Index == 1) this.setState({String1: Event.target.value})
        else this.setState({String2: Event.target.value})
    }

    // =================================================
    // ========             RENDER           ===========
    // =================================================
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
                    <AppHeader Name={"Subsecuencia cómun más larga"} MiniName={"LCS"} />
                </header>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++         SIMULATION             ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <div className="center-align section">
                    <div className="container">
                        <div className="row">
                            <div className="s12">
                                <Simulation String1={this.state.NewString1} String2={this.state.NewString2} />
                            </div>
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
                                    <strong>Cadenas</strong> para la simulación
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
                {/*+++++++++         TUTORIAL MODAL         ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <HotKeys 
                    keyMap   = {{"Send": 'enter'}} 
                    handlers = {{"Send": (e) => M.Modal.getInstance(document.getElementById('TutorialModal')).close()}}>
                    <div id="TutorialModal" className="modal modal-fixed-footer" style={{width:'80%'}}>
                        <div className="modal-content">
                            <div className="container">
                                <h4 className="center-align blue-grey-text text-darken-3">
                                    <strong>Tutorial</strong> para Usar
                                </h4>
                                <br />
                                <div className="row">
                                    <div className="col s10 offset-s1 blue-grey-text text-darken-2">
                                        <p className="flow-text center-align">
                                            Para empezar, esta página es una simulación de una solución al famoso
                                            algoritmo de Longest Common Subsequence mediante Programación Dinámica.

                                            <br />
                                            <br />

                                            Puedes empezar dando click en el botón que dice "Siguiente Paso"

                                            <br />
                                            <br />

                                            Ahora, puedes ir avanzando en la simulación usando la fecha derecha, 
                                            regresar al paso anterior con la flecha izquierda y volver al primer
                                            paso presionando 'r'.

                                            <br />
                                            <br />

                                            Puedes hacer click o poner el mouse sobre una celda para entender que es
                                            lo que representa

                                            <br />
                                            <br />

                                            También puedes abrir el menú con 'ctrl+b' ó 'alt+b'.
                                            Puedes acceder directamente al menú para cambiar los strings con 
                                            'ctrl+c' ó 'alt+c'
                                        </p>
                                    </div>
                                </div>
                            </div>
                        </div>
                        <div className="modal-footer">
                            <a className="btn-flat modal-close waves-effect waves-red red lighten-2">
                                <span className="white-text">Cerrar</span>
                            </a>
                        </div>
                    </div>
                </HotKeys>


            </HotKeys>
            </div>
        )
    }
}



