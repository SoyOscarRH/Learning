import React from "react"
import {Config} from "./Config.jsx"
import {SentData} from "./CoolFunctions.js"


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
            OnConfig: true,
            PrincipalWindow: null
        }

        super(props)

        document.addEventListener('DOMContentLoaded', function() {
            const Elements = document.querySelectorAll('.sidenav')
            const Sidenavs = M.Sidenav.init(Elements, {draggable: true, edge: "left"})
            
            const OptionsModals = {dismissible: true, inDuration: 40, outDuration: 40}
            M.Modal.init(document.getElementById('SubmissionModal'), OptionsModals)
        })
    }



    onClose (State) {
        
        console.log(State) 

        const Waiting = (

            <div className="row">
                <div className="col s6 offset-s3">
                    <br />
                    <br />
                    Analizando tu Petición, por favor espera
                    <br />
                    <div className="progress">
                        <div className="indeterminate"></div>
                    </div>
                </div>
            </div>
        )
        
        this.setState({OnConfig: false, PrincipalWindow: Waiting})


        SentData('/GetTheResult', {State: State})
            .then(Results => {

                console.log(Results)

                const DataResults = Results.Data
                DataResults.shift()

                let NewView = DataResults.map( (Element, index) => (
                    <li key={index}>
                        <div className="collapsible-header">
                            <i className="material-icons">insert_chart</i> Trama
                        </div>
                        <div className="collapsible-body">
                            <p> {Element} </p>
                            <br />
                        </div>
                    </li>
                ))


                NewView = (
                    <ul className="collapsible" style={{whiteSpace: "pre-line"}}>
                        {NewView}                        
                    </ul>
                )

                this.setState({PrincipalWindow: NewView})

                setTimeout( () => {
                    const elems = document.querySelectorAll('.collapsible')
                    const instances = M.Collapsible.init(elems, {})
                }, 200)

                if (State.SaveFile)
                    setTimeout( () =>  {
                        const toastHTML = '<span>Obten tu archivo </span><a class="btn-flat toast-action" href="/download">Descarga</a>'
                        M.toast({html: toastHTML, displayLength: 20000})
                        
                    }, 2000)

            })






    }

    // =================================================
    // ========             RENDER           ===========
    // =================================================
    render () {


        return (
            <div>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++           HEADERS              ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <header>
                    
                    {/*=========================================================*/}
                    {/*================       NAV BAR        ===================*/}
                    {/*=========================================================*/}
                    <div className="navbar-fixed">
                        <nav className="indigo darken-2">
                            <div className="nav-wrapper container">
                                
                                {/*+++++++++++   NAME OF PAGE   ++++++++++++*/}
                                <div className="brand-logo white-text center" style={{fontSize: '1.5rem'}}>
                                    Sniffer
                                </div>

                                {/*+++++++++++   LINK TO HOME   ++++++++++++*/}
                                <a href="/" className="brand-logo right">
                                    <i className="material-icons white-text">home</i>
                                </a>

                                {/*+++++++++++      MENU       ++++++++++++++*/}
                                <a href="#" data-target="SideMenu" className="sidenav-trigger show-on-large">
                                    <i className="material-icons white-text">menu</i>
                                </a>

                            </div>
                        </nav>
                    </div>


                    {/*=========================================================*/}
                    {/*================      SIDE NAV        ===================*/}
                    {/*=========================================================*/}
                    <ul id="SideMenu" className="sidenav">
                        <li className="center">
                            <br />
                            <h5 style={{fontWeight: 300}}>
                                <b>Menú</b> de Opciones
                            </h5>
                            <br />
                        </li>
                        <li><a className="subheader">Tipos</a></li>
                        <li>
                            <a className="waves-effect"
                                href="/"
                            >
                                <i className="material-icons small">home</i>
                                Volver al Inicio
                            </a>
                        </li>
                    </ul>

                </header>

                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                {/*+++++++++         SIMULATION             ++++++++++++*/}
                {/*+++++++++++++++++++++++++++++++++++++++++++++++++++++*/}
                <div className="center-align section">
                    <div className="container">
                        <div className="row">
                            <div className="s12">
                                {this.state.OnConfig && <Config onClose={(State) => this.onClose(State)} /> }
                                {this.state.PrincipalWindow}
                            </div>
                        </div>
                    </div>
                </div>

            </div>
        )
    }
}

