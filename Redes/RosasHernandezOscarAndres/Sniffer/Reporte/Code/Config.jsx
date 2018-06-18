import React from "react"
import {SentData} from "./CoolFunctions.js"

export class Config extends React.Component {

    constructor(props) {
        super(props)
        this.state = {
        	ProgressBar: 10,
        	SaveFile: false,
            NetworkCard: null,
            ByFile: null,
            CurrentView: null
        }
    }

    componentDidMount() {
    	this.ShowSelectType()
    }



    HandleFileSave (Event) {
	    Event.preventDefault()

	    const Data = new FormData()
		const ImageData = document.querySelector('input[type="file"]').files[0]
	    Data.append('file', ImageData)

	    this.setState({ByFile: document.getElementById("UploadInputName").value})

	    fetch('/HandleFile', {method: 'POST', body: Data})
	    .then(response => response.json())
	    .then((response) => {
	    		if (response.Data) M.toast({html: response.Data})
	    		else M.toast({html: "Error: " + response.Data})
	    	}
	    )

	    setTimeout(() => this.props.onClose(this.state), 1000)

    }


    ShowFilter() {
    	const CurrentView = (
    		<div>
	            <div className="row">
					<h6><strong>Ahora vamos a elegir:</strong></h6>
				</div>

				<br />
				<br />
				<div className="row">
					<div className="input-field col s6 offset-s3">
			        	<input id="Filter" type="text" className="validate" />
			        	<label htmlFor="Filter">Filtro para el Sniffer</label>
			        </div>
				</div>

				<div className="row">
					<div className="input-field col s6 offset-s3">
			        	<input id="Size" type="number" className="validate" />
			        	<label htmlFor="Filter">Longitud</label>
			        </div>
				</div>

				<div className="row">
					<div className="input-field col s6 offset-s3">
			        	<input id="TimeOut" type="number" className="validate" />
			        	<label htmlFor="Filter">Tiempo de Escucha</label>
			        </div>
				</div>

				<div className="row">
					<a className="waves-effect waves-light btn" onClick={() => {
						const Filter = document.getElementById("Filter").value
						const TimeOut = document.getElementById("TimeOut").value
						const Size = document.getElementById("Size").value

						const NetworkCard = this.state.NetworkCard
						NetworkCard['Filter'] = Filter
						NetworkCard['Size'] = Size
						NetworkCard['TimeOut'] = TimeOut
						this.setState({NetworkCard: NetworkCard})

						this.props.onClose(this.state)

					}}>Enviar filtros</a>
				</div>
	        </div>
    	)

    	this.setState({CurrentView: CurrentView, ProgressBar: 80})
    }

    ShowSelectNetworkCard () {

    	const CurrentView = (
    		<div className="row">
                <div className="col s6 offset-s3">
                    <br />
                    <br />
                    Cargando Interfaces de Red
                    <br />
                    <div className="progress">
                        <div className="indeterminate"></div>
                    </div>
                </div>
            </div>
    	)
    	this.setState({CurrentView: CurrentView, ProgressBar: 30})


    	const ShowError = (Data) => {
            return (
                <div>
                    <br />
                    <br />
                    <h6>{Data}</h6>
                    <br />
                </div>
            )
        }

        const ShowDevices = (Data) => {

            const ShowCheckboxes = Data.map((Device, index) => {
                
                const SelectMe = () => {
                    const Element = document.getElementById(`Data ${this.state.NetworkCard.Selected}`)
                    if (Element) Element.checked = false

                    this.setState({NetworkCard: {Selected: index} })
                }

                return (
                    <p key={Device} className="left-align">
                        <label>
                            <input type="checkbox" id={`Data ${index}`} onClick={SelectMe}/>
                            <span><strong>{Device[0]}</strong></span>
                            <br />
                            <span>MAC: {Device[2]}</span>
                            <br />
                            <br />

                        </label>
                    </p>
                )
            })

            return (
                <div>
                    <div className="row">
                        <div className="col s8 offset-s2 left-align">
                            {ShowCheckboxes}
                        </div>
                    </div>

                    <div className="row">
                        <a onClick={() => this.ShowFilter()} className="waves-effect waves-light btn-large">Enviar</a>
                    </div>
                </div>
            )
        }

        setTimeout(() => {
            SentData('/GetNetworkInterfaces', {})
            .then(Results => {
                const NewView = (Results.Error != undefined)? 
                    ShowError(Results.Error): ShowDevices(Results.Data)
                this.setState({CurrentView: NewView})
            })
            .catch(ErrorMessageFromServer => console.log(ErrorMessageFromServer))
        }, 1500)
    }

	ShowSelectType () {
		const CurrentView = (
			<div>
		    	<div className="row">
					<h6><strong>Ahora vamos a elegir:</strong></h6>
				</div>

				<br />
				<br />
				<div className="row">

					<div className="card-panel col s10 offset-s1">
						
						<br />
						<h6> Desde una Tarjeta de Red</h6>
						<br />
						<br />
						<a 
							className="waves-effect waves-light btn-large col s6 offset-s3"
							onClick={() => {
								this.setState({NetworkCard: {NetworkCardSelected: null}})
								this.ShowSelectNetworkCard()
							}}
						>
							Escuchar la tarjeta de Red
						</a>

						<br />
						<br />
						<br />
						<br />
						<br />

				 	</div>
				</div>

				<div className="row">

				 	<div className="card-panel col s10 offset-s1">

				 		<br />
						<br />
						<h6> Desde un Archivo</h6>
						<br />

						<form onSubmit={(e) => this.HandleFileSave(e)} method="POST" encType="multipart/form-data">
							<div className="row">
						    	<div className="file-field input-field col s10">
								    <div className="btn">
								        <span>Archivo</span>
								        <input id="UploadInput" type="file" name="UploadInput" />
								    </div>
								    <div className="file-path-wrapper row s12">
								        <input className="file-path validate" type="text" id="UploadInputName" />
								    </div>
							    </div>
							</div>

						    <div className="row">
							    <button className="btn waves-effect waves-light" type="submit" name="action">Enviar
									<i className="material-icons right">send</i>
								</button>
							</div>

 						</form>

					    <br />
						<br />
						<br />

				 	</div>
				</div>

				<br />
				<br />

				<div className="switch">
				<label>
					NO Guardar Archivo
					<input 
						type     = "checkbox"
						onClick = {() => this.setState({SaveFile: !this.state.SaveFile})} />
					<span className="lever"></span>
					Guardar Archivo
					</label>
				</div>

			</div>
		)

		this.setState({CurrentView: CurrentView})
	}


    render () {

    	return (
	    	<div className="row">
		        <div className="col s12">
		            <div className="card-panel blue-grey-text text-darken-2">
		                <h5>Configura tu Sniffer</h5>
						<br />

						<div className="progress">
						    <div className="determinate" style={{width: `${this.state.ProgressBar}%`}}></div>
						</div>

						<br />
						<br />
						
						{this.state.CurrentView}

		            </div>
		        </div>
		    </div>
    	)
    }
}


