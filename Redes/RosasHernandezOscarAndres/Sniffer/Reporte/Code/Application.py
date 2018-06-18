#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||           SALES PAGE       ||||||||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

import os, subprocess
from flask import Flask, render_template, send_file, request, json, session, redirect, url_for
from werkzeug.utils import secure_filename

#==========================================================================
#=================     START AND CONFIGURE THE WEB APP     ================
#==========================================================================

#++++++++++++++++++++++++++++++++++++++++++++
#+++++++          FLASK APP          ++++++++
#++++++++++++++++++++++++++++++++++++++++++++

WebApp = Flask(
            __name__,
            static_folder = "../Static/Distribution",
            template_folder = "../Static",
        )

WebApp.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0
WebApp.config['TEMPLATES_AUTO_RELOAD'] = True
WebApp.config['UPLOAD_FOLDER'] = './Files/'


#==========================================================================
#======================         ROUTES           ==========================
#==========================================================================

#++++++++++++++++++++++++++++++++++++++++++++
#+++++++          ROUT: INDEX        ++++++++
#++++++++++++++++++++++++++++++++++++++++++++
@WebApp.route('/')
def index():
    return render_template("index.html")


#++++++++++++++++++++++++++++++++++++++++++++
#+++++++    DATA FROM BAR CODE       ++++++++
#++++++++++++++++++++++++++++++++++++++++++++
@WebApp.route("/GetNetworkInterfaces", methods=['POST'])
def GetNetworkInterfaces():

    Result = subprocess.check_output("java -jar mostrarDispositivos.jar",shell = True, universal_newlines = True)

    Data = []
    for Device in Result.split("\n"): Data.append(Device.split("@"))
    print(Data)

    return json.dumps({"Data": Data[0:-1]})




#++++++++++++++++++++++++++++++++++++++++++++
#+++++++    DATA FROM BAR CODE       ++++++++
#++++++++++++++++++++++++++++++++++++++++++++
@WebApp.route("/HandleFile", methods=['POST'])
def HandleFile():
    if request.method != 'POST': return json.dumps({"Error": f"Error misterioso"})
    file = request.files['file']

    filename = secure_filename(file.filename)
    file.save(os.path.join(WebApp.config['UPLOAD_FOLDER'], filename))

    return json.dumps({"Data": f"Archivo subido exitosamente"})


#++++++++++++++++++++++++++++++++++++++++++++
#+++++++    DATA FROM BAR CODE       ++++++++
#++++++++++++++++++++++++++++++++++++++++++++
@WebApp.route("/GetTheResult", methods=['POST'])
def GetTheResult():

    if request.json['State']['ByFile'] == None:
        Timeout  = request.json["State"]["NetworkCard"]["TimeOut"]
        Filter   = request.json["State"]["NetworkCard"]["Filter"]
        Selected = request.json["State"]["NetworkCard"]["Selected"]
        Size     = request.json["State"]["NetworkCard"]["Size"]
        Save     = request.json["State"]["SaveFile"]

        Result = subprocess.check_output(f"sudo java -jar ClassicPcapExample.jar {Selected} {Size} {Filter} 0 Mau {Save}",shell = True, universal_newlines = True)
        
        print(Result)
        
        ResultData = Result.split("\n\n")

        return json.dumps({"Data": ResultData})

    else:
        Name  =  f"./Files/{request.json['State']['ByFile']}"
        Save  = request.json["State"]["SaveFile"]

        Result = subprocess.check_output(f"sudo java -jar ClassicPcapExample.jar 0 2048 no 1 {Name} {Save}",shell = True, universal_newlines = True)
        
        print(Result)
       
        ResultData = Result.split("\n\n")

        return json.dumps({"Data": ResultData})




@WebApp.route('/download', methods=['GET', 'POST'])
def download():   

    return send_file("tramas.pcap", as_attachment=True) 


#=============================================================
#================           MAIN        ======================
#=============================================================
if __name__ == "__main__":
    WebApp.run(debug=True)


