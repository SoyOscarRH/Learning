# Diccionario de Datos

## Tablas

### buys

| Campo       | Tipo   | Tamaño máximo | Configuración              | Referencia           | Descripción                            |
| ----------- | ------ | ------------- | -------------------------- | -------------------- | -------------------------------------- |
| id_purchase | Entero | 4 bytes       | Llave foránea <br/>No nulo | purchase.id_purchase | El identiifcador de la compra.         |
| id_drug     | Entero | 4 bytes       | Llave foránea <br/>No nulo | drug.id_drug         | El producto que se compró.             |
| quantity    | Entero | 4 bytes       | No nulo                    |                      | La cantidad de producto que se compró. |



### cashier

| Campo           | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única a un cajerol. |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el empleado. |
| rfc             | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn             | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |



### cleaner

| Campo           | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única a un empleado de intendencia. |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el empleado. |
| rfc             | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn             | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |



### consultation

| Campo            | Tipo     | Tamaño máximo | Configuración                                                | Referencia                   | Descripción                                                  |
| ---------------- | -------- | ------------- | ------------------------------------------------------------ | ---------------------------- | ------------------------------------------------------------ |
| id_consultation  | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |                              | Es el número que sirve para identificar de manera única a la especialidad. |
| patient          | Carácter | 50            | No nulo                                                      |                              | El nombre de la especialidad.                                |
| id_establishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                                   | stablishment.id_stablishment | El establecimiento donde se realizó la consulta              |
| doctor           | Carácter | 18            | Llave foránea <br/>No nulo                                   | doctor.curp                  | El CURP del doctor donde se realiza la consulta              |
| turn             | Entero   | 4 bytes       | No nulo                                                      |                              | El número de turno correspondiente a la consulta.            |
| id_specialty     | Entero   | 4 bytes       | Llave foránea <br/>No nulo                                   | specialty.id_specialty       | El tipo de especialidad que tiene la consultas.              |
| shift            | Enum     | Variable      | No nulo                                                      |                              | El horario en que se realizó la consulta, toma valores 'moorning' (mañana) o 'afternoon' (tarde). |
| total_cost       | Númerico | Variable      | No nulo                                                      |                              | El costo actual de la consulta con respecto a esa especialidad. |
| date             | Fecha    |               | No nulo                                                      |                              | La fecha en que se realizó la consulta. Tiene formato yyyy-mm-dd |



### customer

| Campo      | Tipo     | Tamaño máximo | Configuración                                       | Referencia  | Descripción                                                  |
| ---------- | -------- | ------------- | --------------------------------------------------- | ----------- | ------------------------------------------------------------ |
| curp       | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp | El CURP que sirve para identificar de manera única al cliente. |
| is_patient | Booleano | 1 byte        | No nulo                                             |             | Señala si el cliente es un paciente (ha tomado alguna consulta) o no lo es. |



### deliveryman

| Campo           | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única a un repartidor. |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el empleado. |
| rfc             | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn             | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |
| license         | Caŕacter | 11            |                                                     |                              | La licencia de conducir del repartido, en caso de que se necesite. |
| vehicle         | Enum     | Variable      | No nulo                                             |                              | El vehículo ocupado por el repartido, toma los valores 'bike' (bicicleta) o 'motorbike' (motocicleta) |



### doctor

| Campo                | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| -------------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp                 | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única a un doctor. |
| id_stablishment      | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el empleado. |
| rfc                  | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn                  | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |
| professional_license | Carácter | 8             | No nulo                                             |                              | La licencia profesional del médico.                          |



### drug

| Campo             | Tipo     | Tamaño máximo | Configuración                                                | Referencia | Descripción                                                  |
| ----------------- | -------- | ------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| id_drug           | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |            | Es el número que sirve para identificar de manera única al fármaco. |
| name              | Carácter | 400           | No nulo                                                      |            | El nombre del fármarco.                                      |
| presentation      | Carácter | 100           | No nulo                                                      |            | En que formato está presentado el fármaco, por ejemplo, en tables, gotas, etc. |
| brand             | Carácter | 100           | No nulo                                                      |            | La marca al que pertenece el fármaco.                        |
| price             | Númerico | Variable      | No nulo                                                      |            | El costo actual deñ farmaco.                                 |
| need_prescription | Booleano | 1 byte        | No nulo                                                      |            | Marca si es necesario una receta médica para comprar este medicamento. |



### ewallet

| Campo          | Tipo     | Tamaño máximo | Configuración                   | Referencia    | Descripción                                                  |
| -------------- | -------- | ------------- | ------------------------------- | ------------- | ------------------------------------------------------------ |
| ewallet_number | Carácter | 18            | **Llave primaria**<br />No nulo |               | El número correspondiente al monedero electrónico de un cliente. |
| curp           | Carácter | 18            | Llave foránea <br/>No nulo      | customer.curp | El CURP correspondiente al cliente.                          |
| balance        | Númerico | Variable      | No nulo                         |               | El saldo actual del cliente en el monedero.                  |



### general

| Campo           | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única a un empleado general. |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el empleado. |
| rfc             | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn             | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |



### has

| Campo       | Tipo   | Tamaño máximo | Configuración              | Referencia           | Descripción                                                  |
| ----------- | ------ | ------------- | -------------------------- | -------------------- | ------------------------------------------------------------ |
| id_purchase | Entero | 4 bytes       | Llave foránea <br/>No nulo | purchase.id_purchase | El identiifcador de la compra.                               |
| invoice     | Entero | 4 bytes       | Llave foránea <br/>No nulo | prescription.invoice | La receta asociada a la compra y los productos que necesitan receta para ser adquiridos. |



### indicate

| Campo   | Tipo   | Tamaño máximo | Configuración              | Referencia           | Descripción                                                  |
| ------- | ------ | ------------- | -------------------------- | -------------------- | ------------------------------------------------------------ |
| invoice | Entero | 4 bytes       | Llave foránea <br/>No nulo | prescription.invoice | El identiifcador de una receta.                              |
| id_drug | Entero | 4 bytes       | Llave foránea <br/>No nulo | drug.id_drug         | El medicamento que se preescribe en la receta.               |
| dosis   | Entero | 2 bytes       | No nulo                    |                      | El número de dosis que tiene que tomar el paciente cada cierta hora. |
| hours   | Entero | 2 bytes       | No nulo                    |                      | El tiempo entre cada dosis en horas.                         |



### is_composed

| Campo        | Tipo   | Tamaño máximo | Configuración              | Referencia             | Descripción                                                  |
| ------------ | ------ | ------------- | -------------------------- | ---------------------- | ------------------------------------------------------------ |
| id_drug      | Entero | 4 bytes       | Llave foránea <br/>No nulo | drug.id_drug           | El identificador del fármaco, que sirve para relacionarlo con las sustancias que lo componen. |
| id_substance | Entero | 4 bytes       | Llave foránea <br/>No nulo | substance.id_substance | El nombre de la sustancia.                                   |



### manager

| Campo           | Tipo     | Tamaño máximo | Configuración                                       | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | --------------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />Llave foránea <br />No nulo | person.curp                  | El CURP que sirve para identificar de manera única al encargado de un establecimiento. |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo                          | stablishment.id_stablishment | El identificador del establecimiento donde trabaja el encargado. |
| rfc             | Carácter | 13            | No nulo                                             |                              | El RFC correspondiente al empleado.                          |
| ssn             | Carácter | 11            | No nulo                                             |                              | El Número de Seguridad Social correspondiente al empleado.   |



### offers

| Campo        | Tipo     | Tamaño máximo | Configuración              | Referencia             | Descripción                           |
| ------------ | -------- | ------------- | -------------------------- | ---------------------- | ------------------------------------- |
| curp         | Carácter | 18            | Llave foránea <br/>No nulo | doctor.curp            | El CURP del doctor.                   |
| id_substance | Entero   | 4 bytes       | Llave foránea <br/>No nulo | specialty.id_specialty | La especilidad que atiende el doctor. |



### person

| Campo           | Tipo     | Tamaño máximo | Configuración                   | Referencia | Descripción                                                  |
| --------------- | -------- | ------------- | ------------------------------- | ---------- | ------------------------------------------------------------ |
| curp            | Carácter | 18            | **Llave primaria**<br />No nulo |            | El CURP correspondiente a una persona, que es único.         |
| name            | Carácter | 100           | No nulo                         |            | El nombre de la persona.                                     |
| first_lastname  | Carácter | 100           | No nulo                         |            | El apellido paterno de la persona.                           |
| second_lastname | Carácter | 100           | No nulo                         |            | El apellido materno de la persona.                           |
| gender          | Enum     | Variable      | No nulo                         |            | El género de la persona. Toma valores 'male' (hombre), 'female' (mujer), 'other' (otro). |
| phone           | Carácter | 13            | No nulo                         |            | El teléfono de la persona.                                   |
| street          | Carácter | 100           | No nulo                         |            | La calle donde vive la persona.                              |
| street_number   | Entero   | 4 bytes       | No nulo                         |            | El número de casa donde vive la persona.                     |
| email           | Carácter | 100           | No nulo                         |            | El correo electrónico de la persona.                         |
| zip_code        | Carácter | 5             | Llave foránea<br/>No nulo       | zip.code   | El código postal correspondiente a donde vive la persona.    |



### phone_number

| Campo           | Tipo     | Tamaño máximo | Configuración              | Referencia                   | Descripción                                                  |
| --------------- | -------- | ------------- | -------------------------- | ---------------------------- | ------------------------------------------------------------ |
| id_stablishment | Entero   | 4 bytes       | Llave foránea <br/>No nulo | stablishment.id_stablishment | El identiifcador del establecimiento al cual le corresponde tal número telefónico. |
| phone           | Carácter | 13            | No nulo                    |                              | El número de telefóno del establecimiento                    |



### prescription

| Campo                  | Tipo     | Tamaño máximo | Configuración                                                | Referencia                   | Descripción                                                  |
| ---------------------- | -------- | ------------- | ------------------------------------------------------------ | ---------------------------- | ------------------------------------------------------------ |
| invoice                | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |                              | Es el folio de la receta.                                    |
| id_consultation        | Entero   | 4 bytes       | Llave foránea <br/>No nulo                                   | consultation.id_prescription | El identificador de la consulta donde se genero la receta.   |
| next_appointment       | Date     |               |                                                              |                              | La fecha de la proxíma visita si hay. Sigue el formato yyyy-mm-dd |
| digital_signature      | Carácter | 65,535 bytes  | No nulo                                                      |                              | La firma digital de la receta.                               |
| additional_indications | Carácter | 65,535 bytes  |                                                              |                              | Indicaciones adicionales que agregue el doctor.              |



### purchase

| Campo            | Tipo     | Tamaño máximo | Configuración                                                | Referencia                     | Descripción                                                  |
| ---------------- | -------- | ------------- | ------------------------------------------------------------ | ------------------------------ | ------------------------------------------------------------ |
| id_purchase      | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |                                | Es el número que sirve para identificar de manera única a una compra. |
| cashier          | Carácter | 18            | Llave foránea <br/>No nulo                                   | cashier.curp                   | El cajero que realizó la venta.                              |
| customer         | Carácter | 18            | Llave foránea <br/>No nulo                                   | customer.curp                  | El cliente que realizó la compra.                            |
| id_establishment | Numérico | 4 bytes       | Llave foránea <br/>No nulo                                   | establishment.id_establishment |                                                              |
| timestamp        |          | 18            | No nulo                                                      |                                | La fecha y hora del momento en que se realizo la compra.     |
| payment_method   | Enum     | Variable      | No nulo                                                      |                                | El método de pago ocupado para la compra, el cual tiene valores 'cash' (efectivo), 'debitcard' (tarjeta de debito), 'ewallet' (monedero electrónico). |
| total            | Númerico | Variable      | No nulo                                                      |                                | El total de la compra realizada.                             |



### substance

| Campo        | Tipo     | Tamaño máximo | Configuración                                                | Referencia | Descripción                                                  |
| ------------ | -------- | ------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| id_substance | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |            | Es el número que sirve para identificar de manera única a una sustancia que puede contener un fármaco. |
| name         | Carácter | 50            | No nulo                                                      |            | El nombre de la sustancia.                                   |



### specialty

| Campo        | Tipo     | Tamaño máximo | Configuración                                                | Referencia | Descripción                                                  |
| ------------ | -------- | ------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| id_specialty | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |            | Es el número que sirve para identificar de manera única a la especialidad. |
| name         | Carácter | 50            | No nulo                                                      |            | El nombre de la especialidad.                                |
| current_cost | Númerico | Variable      | No nulo                                                      |            | El costo actual de la consulta con respecto a esa especialidad. |



### stablishment

| Campo           | Tipo     | Tamaño máximo | Configuración                                                | Referencia | Descripción                                                  |
| --------------- | -------- | ------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| id_stablishment | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |            | Es el número que sirve para identificar de manera única a un establecimiento. |
| street          | Carácter | 50            | No nulo                                                      |            | La calle donde está ubicado el establecimiento.              |
| street_number   | Entero   | 4 bytes       | No nulo                                                      |            | El número de calle que le corresponde al establecimiento.    |
| zip_code        | Carácter | 5             | Llave foránea<br/>No nulo                                    | zip.code   | El código postal que corresponde a la ubicación del establecimiento. |



### state

| Campo    | Tipo     | Tamaño máximo | Configuración                                                | Referencia | Descripción                                                  |
| -------- | -------- | ------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| id_state | Entero   | 4 bytes       | **Llave primaria**<br />Autoincrementable <br/>No nulo <br />Generado como identidad |            | Es el número que sirve para identificar de manera única al estado. |
| name     | Carácter | 31            | No nulo                                                      |            | El nombre del estado.                                        |



### zip

| Campo    | Tipo     | Tamaño máximo | Configuración                    | Referencia     | Descripción                                                  |
| -------- | -------- | ------------- | -------------------------------- | -------------- | ------------------------------------------------------------ |
| code     | Cáracter | 5             | **Llave primaria** <br />No nulo |                | Es el número del código postal                               |
| id_state | Entero   | 4 bytes       | Llave foránea <br />No nulo      | state.id_state | El identificador del estado al que corresponde el código postal |
| neighbor | Carácter | 50            | No nulo                          |                | El nombre de la colonia al que pertenece el código postal.   |
| city     | Cáracter | 40            |                                  |                | El nombre de la cuidad al que pertenece el código postal     |

