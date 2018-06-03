import React from "react";





// =====================================================================
// ============     HEADER COMPONENT       =============================
// =====================================================================
export default class AppHeader extends React.Component {

    constructor(props) {
        super(props)

        const Name = (window.innerWidth <= 800)? this.props.MiniName: this.props.Name
        this.state = {HeaderName: Name}

        window.onresize = () => {
            const Name = (window.innerWidth <= 800)? this.props.MiniName: this.props.Name
            this.setState({HeaderName: Name})
        }

        document.addEventListener('DOMContentLoaded', function() {
            const Elements = document.querySelectorAll('.sidenav')
            const Sidenavs = M.Sidenav.init(Elements, {draggable: true, edge: "left"})
        })
    }

    render () {


        return (
            <div>
                {/*=========================================================*/}
                {/*================       NAV BAR        ===================*/}
                {/*=========================================================*/}
                <div className="navbar-fixed">
                    <nav className="indigo darken-2">
                        <div className="nav-wrapper container">
                            
                            {/*+++++++++++   NAME OF PAGE   ++++++++++++*/}
                            <div className="brand-logo white-text center" style={{fontSize: '1.5rem'}}>
                                {this.state.HeaderName}
                            </div>

                            {/*+++++++++++   LINK TO HOME   ++++++++++++*/}
                            <a href="/index.html" className="brand-logo right">
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
                    <li><a className="subheader">General</a></li>
                    <li>
                        <a className="waves-effect"
                            onClick={(e) => M.Modal.getInstance(document.getElementById('SubmissionModal')).open()}>
                            <i className="material-icons small">create</i>
                            Cambiar Cadenas
                        </a>
                    </li>
                    <li>
                        <a className="waves-effect"
                            onClick={(e) => M.Modal.getInstance(document.getElementById('TutorialModal')).open()}>
                            <i className="material-icons">info</i>
                            Tutorial de Uso
                        </a>
                    </li>
                </ul>
            </div>
        );
    }
}