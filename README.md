# Clinical trial participation for vulnerable cancer patients in Denmark and England

This repo contains a guide to: 
  1) Download simulacrum
  2) Set up a oracle express database as a docker image 
  3) Download Dbeaver database manager
  4) Download and setup ODBC software for R
  5) Create a odbc connection to R studio

Please notice that in our example that R have been chosen as the programming language for the model. Oracle Express have been chosen as the database. Dbeaver have been chosen as database manager. 
Except the database manager, there are restriction for what software can be used for access the Simulacrum service. 

### Disclaimers
  1) Disclaimer: Use the newest version of Simulacrum. The newest version if what resemples the real data the most. Do not use the old versions, if you want to access the real data.
  2) Disclaimer: NDRS (Provider of the real cancer data) do only offer 3 hours of free run time for a model. This provides limitations to more complex models and this also means that one have to be sure that the script works on the simulacrum data
                Before it is shipped to the actual data.
  3) Be aware of NDRS best practice for analysing the cancer data.

### Material 
Simulacrum and NDRS offers information and guides on how to access the real data through Simulacrum while also offers guide to the usage of Simulacrum. 
Some of the recommended documentation are: 
  1) https://simulacrum.healthdatainsight.org.uk/
  2) https://simulacrum.healthdatainsight.org.uk/wp-content/uploads/2023/04/Simulacrum-v2-User-Guide.pdf
  3) https://simulacrum.healthdatainsight.org.uk/wp/wp-content/uploads/2018/11/SQL-Query-Guide.pdf

## Simulacrum tutorial and overview 
  1) Download Simulacrum (Newest version (currently v2.1.0(06-06-2024)))
  2) Download a Docker desktop: https://www.docker.com/products/docker-desktop/
  3) Download Oracle-xe docker image: https://hub.docker.com/r/gvenzl/oracle-xe
     3.1) Follow the instruction under the section "Quick Start" in the provided link
  4) Download Dbeaver database manager: https://dbeaver.io/
     4.1) Follow the instructions in this link: https://dbeaver.com/docs/dbeaver/Create-Connection/
  5) Download ODBC driver for oracle through this link: https://download.oracle.com/otn_software/nt/instantclient/instantclient-odbc-windows.zip
     5.1) Follow the instructions in this video: https://www.youtube.com/watch?v=vSrsfJ6nkuo&t=99s
  6) Open R studio and make it create a connection string for the oracle Database (Please see "analysis_ODBC_2024_v2.1_odbc.R" for a example of how a connection string can look like
  7) Start writing your sql query
