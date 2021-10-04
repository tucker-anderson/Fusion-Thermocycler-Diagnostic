# Thermocycler-Diagnostic

## About

This is a Shiny web application to help determine Thermocycler LED decay or other issues. The application takes in Thermocycler fluoresence reads from Panther Service Software (a PEEK lid scan and an open airbackground scan) and outputs an excel worksheet file with well summaries.

## Installation / Access

This application is hosted on the http://sdbioinfoapp06/ server and managed through portainer. The docker images can be accessed on the Hologic local network (must be logged in or VPN'd) at 

- <http://10.200.254.74:27211/app/> for the production version of this container
- <http://10.200.254.74:27212/app/> for the development version of this container.

To run locally, build docker image from dockerfile and start docker container, then navigate to <http://localhost:8888/> in your web browser.

## Instructions on Use

1. Enter Panther and Thermocycler serial numbers in their respective fields.
2. Select your Peek scan file from service software using the browse command.

    > At this point, the peek lid presence option, peek sheet fluorometer values and peek lid fluorometer barcodes

3. Select your Background scan file from service softwre using the browse command.
4. IF files are successfully uploaded, Calculate option should be enabled.
5. After clicking the Calculate option, the Download option should be available to transfer the excel workbook to your local machine.

## Release Notes

#### v1.0.0.0

- initial release

#### v1.0.0.1

- Service Software scan files with quotes (") will no longer break application
- output now includes a raw background subtracted table
- include all tables / tabs in output regardless of PEEK lid selection
