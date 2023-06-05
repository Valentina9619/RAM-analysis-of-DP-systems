# Solicitamos las librerias que necesitaremos
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(DT)
library(mathjaxr)
library(plotly)

#Data
dataRandF <- data.frame(
  Months = c(0, 1, 3, 6, 9, 12),
  Probability = c(0, 0.004, 0.024, 0.1067, 0.2547, 0.4236, 1, 0.9960, 0.9760, 0.8933, 0.7453, 0.5764),
  Indicator = c("Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability",
                "Reliability", "Reliability", "Reliability", "Reliability", "Reliability", "Reliability")
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  #Application title
                  titlePanel("Reliability, Availability and Maintainability Analysis of Dynamic Positioning Systems in Well Stimulation Vessels"),
                  navbarPage(" ",
                             tabPanel("Home", icon = icon("home"),
                                      fluidRow(column(br(),
                                                      tags$img(src = "DP_System.JPG", width="210px", height="200px"), 
                                                      width = 2),
                                               column(br(),
                                                      p("Due to the high demand for hydrocarbons as the major source of energy, the oil and gas
                                                      industry has expanded beyond the shore. Subsequently, exploration and production in the 
                                                      offshore oil and gas industry has moved into deeper waters (Lehmkoster, 2014). The challenging 
                                                      environment of deep waters has required the adoption of more modern and advanced technology
                                                      and equipment. One of the significant problems has been maintaining the vessel or floating 
                                                      platform's position for carrying out operations. This has made operations either unfeasible 
                                                      or very costly because of the costs of hiring the anchor handling vessels (Rappini, S., Pallaoro, A., & Heringer, 2003). The
                                                      Dynamic Positioning (DP) System has, therefore, been introduced as one of the modern technologies 
                                                      used to solve the positioning problems of deep-water vessels/platforms.",
                                                        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                      width = 8),
                                               column(
                                                   br(),
                                                   tags$img(src="LabRisco.PNG",width="200px",height="60px"),
                                                   br(),
                                                   br(),
                                                   p("For more information please check the",
                                                     em("Analysis, Evaluation and Risk Management Laboratory"), "page clicking",
                                                     br(),
                                                     a(href="https://www.labrisco.usp.br/home_br", "Here",target="_blank"),
                                                     style = "text-align:center;color:black"),
                                                   width = 2)
                                               ),
                                      br(),
                                      fluidRow(column(tags$img(src="DP.jpg", width="210px", height="200px"), width = 2),
                                               column(
                                                   br(),
                                                   p("This app presents a particular configuration of a", strong("DP system"), "used by a Class Z Diesel Electric 
                                                 well stimulation vessel. The vessel is classified as DPS-2 by the ABS (American Bureau of Shipping), based on the 
                                                 2014 ABS Guide for Dynamic Positioning Vessels (ABS, 2013) and the rules for construction and classification of 
                                                 offshore support vessels. 
                                                 
                                                 
                                                 In the ", strong("App"), "you could find the functional analysis results, the most important findings of reliability analysis
                                                   as well as the critical compenents identified from Importance Measures. Furthermore, in the tab Maintainability Analysis, 
                                                   the repair times simulates are exposed and in Availability Analysis the reader could find the 90% confidence level of
                                                   availability of the system.",
                                                     style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                   width = 8),
                                               hr(),
                                               hr(),
                                               hr(),
                                               p(em("Developed by"), br("Maria Valentina Clavijo Mesa"), style = "text-align:center; font-family: times"))
                                      ), #Finish Home
                             tabPanel("Functional Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("System Description"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 2, icon("hand-point-right","fa-5x"), align = "center"),
                                               column(
                                                 p("The DP System comprises three main subsystems: power, thruster, and control. These three subsystems work in
                                                 unison to maintain the vessel heading and position by controlling the movement of the vessel. In addition, 
                                                 there is a set of six auxiliary subsystems that complement the operation of the main subsystems.
                                                 
                                                 The ",strong("power subsystem")," comprises all the components necessary to supply the DP system with electric 
                                                 power. 
                                                 
                                                 The", strong("thruster subsystem"), " involves all the components needed to supply the DP system with thrust 
                                                 force and direction.
                                                 
                                                 The ", strong("control subsystem"), " comprises all components and related systems, required hardware and 
                                                 software to dynamically command the vessel position.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 p("The figure shows the general configuration of the main subsystems in a DP vessel and the three main 
                                                   subsystems described above [power (red), thruster (yellow) and control (green)]. Notice that the auxiliary
                                                   subsystems do not appear in this figure, since its operation is to support the main components (mover-generators
                                                   and thrusters), because the purpose here is to show only a general configuration.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 tags$img(src="Functional_Description.JPG",width="600px",height="600px", 
                                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                                 br(),
                                                 br(),
                                                 p("As can be seen from figure, the DP system power is produced in mover-generators and then arrives at the switchboard,
                                                   where it is reduced to the control and the thruster subsystems.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 p("In order to supply power to the control subsystem equipment, the switchboard filters power through UPSs to absorb 
                                                   current spikes (Rappini, S., Pallaoro, A., & Heringer, 2003). In the event that the mover-generators fail, an emergency power source will provide power
                                                   to the UPSs in order to maintain control of vital components. Nevertheless, it should be noted that the emergency 
                                                   mover-generator (the reserve mover-generator) would not have sufficient power for running the thrusters (Rasoulzadeh, 2015).
                                                   This is due to high power consumption of thrusters.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 p("In relation to the flow of information, the control computers receive information from two sources: the environment 
                                                   sensors (Gyro, Wind and VRS - Vertical Reference Sensor) and PRS - Position Reference System. The computer processes 
                                                   all the information and sends signals to the electronic controls of the thruster subsystem. The DP vessels should have
                                                   an alternative manual control of the thrusters for the case where all control computers fail; to ensure this, there is 
                                                   an IJS - Independent Joystick System, which is directly connected to the electronic controls of the thruster subsystem, 
                                                   and the UPS powers it. Additionally, as previously commented, the IJS receives information from some of the environment
                                                   sensors and PRS.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 p("Finally, once the information and power are transmitted to the electronic controls of the thruster subsystem, these 
                                                   equipment send the information to the thrusters in order to reach the directions and commands previously established, 
                                                   and to obtain control of the vessel position.", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                   p("In the case where the reader wishes to study more literature reviews about DP Systems, some references are recommended
                                                     as (EBrahimi, 2010), (Vedachalam and Ramadass, 2017), and (Clavijo, et al., 2020)", 
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 2),
                                               column(p("Let's do it. You are going to find the functional diagrams of the DP System analyzed, in order to identify 
                                                         technical and operational characteristics of system. Furthermore, Failure and Repair data used in the analysis will be 
                                                         presented.", style = "color:black;text-align:center"),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                               
                                      ),
                                      br(),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("Functional Diagram and Collected Data"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      hr(),
                                      tabsetPanel(
                                        tabPanel("Functional Diagrams",
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("System",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="SSB.jpg",width="750px",height="900px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       )),
                                                                       br()),
                                                              tabPanel("Operational Characteristics",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Some operational conditions that must be considered in the study are: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- Regarding the power subsystem, the analysis considers that all mover-generators are working and connected, that is, 
                                                                                    the mover-generator redundancy obeys a parallel system sharing load. Bus-bar 1 operates open (connection between MVS1 and MVS3), 
                                                                                    while bus-bar 2 operates closed (connection between MVS2 and MVS3).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- In the propulsion subsystem, all thrusters are connected and ready to operate.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- In the control subsystem, the alternative station (BU OS2) is used when all main control computers (control stations OS1 and 
                                                                                    OS2) fail.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- It is assumed that the operator performs his duties in accordance with established operating procedures
                                                                                    (human reliability is not assessed).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=10,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )), br()),
                                                              tabPanel("Technical Characteristics",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Some operational conditions that must be considered in the study are: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- Power is generated by four mover-generators, which supply the sections of the main switchboard
                                                                                    (MVS 1, MVS 2 and MVS 3). Note that MG 1 and 2 supply power to MVS 1 while MG 3 and 4 energize MVS 2", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The three sections supply power to the thrusters, the MVS 1 supplies power to the port Z-Drive (PZD) 
                                                                                    thruster and Tunnel 2 (BTT2), while the MVS 2 supplies power to the starboard Z-Drive (SZD) and to the Drop Down 
                                                                                    thruster (DDT). In turn, MVS 3 energizes Tunnel 1 thruster (BTT1). In short, the MG 1 and MG 2 power the PZD and BTT1
                                                                                    thrusters while the MG 3 and MG 4 power the SZD, BTT2 and DDT thrusters.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- From the starboard transformer that is connected to the LVS 2, the LP - Lighting Panel, is powered in normal 
                                                                                    condition. However, in the event of a loss of power from the LVS 2, an auto transfer switch will automatically switch 
                                                                                    to the supply feed from the port transformer, resulting in only a temporary loss of consumers supplied exclusively from 
                                                                                    the LP.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- UPS 1 and 2 have two power supplies, one primary (LP) and one secondary (ELP), while UPS 3 has only one power source
                                                                                    from the ELP.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Regarding the information flow, there are three control stations: two main (OS1 and OS2) and a backup (BU OS2). All 
                                                                                    have a control computer except OS2 which have two computers.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The Gyro 1, Wind 1, and VRU 1 environmental sensors send exclusive information to OS1, while OS2 receives exclusive 
                                                                                    information from the Gyro 2, Wind 2 and VRU2 environmental sensors. The third redundancy of environmental sensors (Gyro 3, 
                                                                                    Wind 3 and VRU3) share information for the two main control stations (OS1 and OS2).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Position reference systems DGPS1 and LADAR send information to OS1 at the same time as DGPS2 and RADAR send information 
                                                                                    to OS2. It should be noted that Gyro 3 is the only source of information from the backup control station (BU OS2).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The specific configuration does not show the equipment of the auxiliary subsystems. However, according to the FMEA, the 
                                                                                    failure of the cooling, fuel and/or lubrication subsystem may have an effect on the vessel's positioning, given that these 
                                                                                    auxiliary subsystems are linked to the active equipment of the power and propulsion subsystem. In turn, for the compressed 
                                                                                    air and heating, ventilation and air conditioning subsystems, the FMEA shows that the failure modes associated with these 
                                                                                    subsystems do not affect the vessel's positioning.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=10,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )),
                                                                       br()),
                                                              tabPanel("Functional Tree",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="General_Functional_Tree.jpg",width="970px",height="300px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12)
                                                                         ), width = 12
                                                                       )),
                                                                       br()
                                                                       ))),
                                        tabPanel("Data",
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("Failure Data",
                                                                       tags$style(".fa-database {color:#227fe8}"),
                                                                       h3(p(em("Failure Dataset "), icon("database",lib = "font-awesome"), style ="color:black;text-align:center")),
                                                                       fluidRow(column(br(),DT::dataTableOutput("FRawData"), width = 11)
                                                                       )),
                                                              
                                                              tabPanel("Repair Data",
                                                                       tags$style(".fa-database {color:#227fe8}"),
                                                                       h3(p(em("Repair Dataset "), icon("database",lib = "font-awesome"), style ="color:black;text-align:center")),
                                                                       fluidRow(column(br(),DT::dataTableOutput("RRawData"), width = 11)
                                                                       ))
                                                 ))
                                      )
                                      ), #Finish Functional Analysis
                             tabPanel("Reliability Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Reliability"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, tags$img(src="Rcurve.JPG",width="300px",height="250px", 
                                                                          style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                               column(
                                                 p("Reliability has many connotations. In general, it refers to the ability of an item (a product or a system) to
                                                 perform its", em("function")," under designed operating ", em("conditions")," for a designated", em("period of 
                                                 time or number of cycles"), " (Modarres, Kaminskiy, & Krivtsov, 2009). The ability of an item to perform its function is normally
                                                 designated through a", em("probability"), " (the probabilistic connotation). The probabilistic treatment of an item reliability, according
                                                 to the definition above, can be summarized by", style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$R(~t~)=(T \\geq T'|C_1, C_2, ... , C_n)$$",style="color:black;border:1px solid black;background-color:white"),
                                                 p("Where", em("T'")," is the designated period of time or number of cycles for the item's operation (e.g., mission time) when time
                                                 or cycle of application is the aggregated agent of failure and is the strength, endurance limit, or performance requirements when 
                                                 stress-strength, damage-tolerance or performance-requirements models are used.", em("T")," is the time to failure or cycle to failure 
                                                 when time or application cycle is the agent of failure and is the stress, amount of damage, or performance of the item when stress-strength,
                                                 damage-tolerance, or performance-requirements models are used.", em("R(t)"), " is the reliability of the item at time or application cycle t
                                                   after which the mission is completed, and", em("C1, C2, ... , Cn"), "are the designated conditions, such as environmental conditions.",
                                                   style = "color:black;text-align:justify"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      h1(),
                                      fluidRow(column(width = 3),
                                               column(p("Particularly for the analysis of the DP system, the FTA - Fault Tree Analysis was the reliability technique used. The FTA for the system is detailed 
                                               below, as well as the reliability and non-reliability results estimated from the failure rates collected (see Data section of", em(" Functional Analysis)."), 
                                                        style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                               
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Reliability Assessment for The DP System"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      hr(),
                                      tabsetPanel(
                                        tabPanel("Qualitative Analysis",
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("Fault Tree of The Interest Event",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="Top_FTA.JPG",width="750px",height="450px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       )
                                                                       ),
                                                                       br()),
                                                              tabPanel("Fault Tree of Power Subsystem",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="FTA_Power.jpg",width="750px",height="600px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       )),
                                                                       br()),
                                                              tabPanel("Fault Tree of Thruster Subsystem",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="FTA_Thruster.jpg",width="600px",height="750px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       )),
                                                                       br()),
                                                              tabPanel("Fault Tree of Control Subsystem",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="FTA_Control.jpg",width="550px",height="750px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       )),
                                                                       br()),
                                                              tabPanel("Description of Fault Trees",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("There ate three main failures that can lead the ship to a total loss of power for the DP 
                                                                                                system equipment. Thus, if any of the following events occur, the Power Subsystem will fail: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- A power generation failure, which occurs if the port side (MG1 and MG2) and starboard side (MG3 and MG4)
                                                                                    mover-generators fail simultaneously. Note that each mover-generator has an independent cooling, fuel and 
                                                                                    lubrication subsystem. Thus, individual failures in these subsystems lead to the loss of the respective 
                                                                                    dedicated mover-generator. Therefore, in this failure event", em(" (power generation failure),")," for each mover-generator,
                                                                                    the failure events associated with the auxiliary subsystems are also considered.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Failure of the Medium Voltage Switchboards is other event that would lead to a total failure of the power subsystem.
                                                                                    That is, if there is a simultaneous short circuit in MVS1 and a short circuit in one of the starboards MVS's (MVS2 and MVS3),
                                                                                    the ship would lose its position because the electrical installation that supplies energy to the thrusters would fail. It is
                                                                                    worth noting that the OR gate between sections MVS2 and MVS3 models the fact that these sections operate with a closed bus-bar,
                                                                                    therefore, if one of the sections is short-circuited, the other will also be in fault.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Total loss of power supply to the auxiliary subsystems could also generate a total power susbystem failure. 
                                                                                    This failure occurs when the main power supplies for the auxiliary subsystems equipment are lost and the emergency equipment
                                                                                    simultaneously loses the capacity to generate power.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )),
                                                                       br(),
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Regarding the Thruster Subsystem... "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("The total failure of this subsystem will happen if the vessel loses the ability to maintain its 
                                                                                    position in the longitudinal axis or in the transverse axis. This happens when there is a loss of all 
                                                                                    bow thrusters or all stern thrusters.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("The loss of the PZD and SZD stern thrusters will make the ship unable to maintain its position in 
                                                                                    the longitudinal axis, as the tunnel thrusters (BTT1 and BTT2) operate in the transverse axis. Even 
                                                                                    though the DDT thruster is still available, the thrust of this thruster is not enough to maintain the 
                                                                                    ship's position. On the other hand, if there is a loss of the bow thrusters (BTT1, BTT2 and DDT), the 
                                                                                    ship will not be able to maintain the transverse position, as it is necessary to use thrusters in the 
                                                                                    bow and stern to ensure the transverse displacement. Furthermore, the power is not enough to perform 
                                                                                    the displacement. ", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )),
                                                                       br(),
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Finally, if any of the situations detailed below occur, the unit will present a total failure
                                                                                                in the Control Subsystem: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- The control subsystem equipment loses power supply. It is worth noting that according FMEA of vessel and 
                                                                                    the specific configuration designed", em("(see System diagram in Functional Analysis tab),"), " the environmental 
                                                                                    sensors, the Position Reference Systems and the control computers receive power through the UPSs. Therefore, if 
                                                                                    these fail, the sensors and control computers lose power supply. Note that the FT does not include the ELP and LP 
                                                                                    power sources as even when these fail, the UPS should be able to continue operation for 30 minutes (Rappini, S., 
                                                                                    Pallaoro, A., & Heringer, 2003), (Navship, 2016).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- If all control stations fail (OS1, OS2 and BU-OS2). Each of the control stations may fail in case of loss of 
                                                                                    environmental information (due to failure of Gyro, Wind or VRU sensors), loss of position reference information 
                                                                                    or if computers control systems fail, which are responsible for evaluating the information collected by the 
                                                                                    environmental sensors and the position reference systems and establishing with these ifnormations the target 
                                                                                    coordinates for each one of the thrusters.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )),
                                                                       br())
                                                 )
                                                 ),
                                        tabPanel("Quantitative Analysis",
                                                 fluidRow(br(),
                                                          br(),
                                                          column(width = 1), 
                                                          column(
                                                            fluidRow(plotlyOutput("Prueba")), 
                                                            width = 8),
                                                          column(h3("Note that..."),
                                                                 p("Considering a reliability of 95% as a reference, the suggested period for the general evaluation of the vessel to return to 
                                                                   the condition of 'as good as new' is approximately 4 months.", 
                                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                 br(), width=3,style="background-color:lavender;border-radius: 10px")
                                                 ))
                                      )
                                      ), #Finish Reliability Analysis
                             tabPanel("Importance Measures", icon = icon("bookmark"),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("Importance Measures - IM"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                        p("IM are numeric tools to rank critical equipment. The choice of IM depends not only on the objective of the analysis, but also on the available data. In the 
                                          literature the IM had been discussed and different contexts (Sudarmono, 2019), (Makajic-Nikolic et al., 2018), (Si et al., 2010) and (Salazar et al., 2016). 
                                          In this way Van Der Bost and Schoonakker (2001), shows a classification of different IM according with the data required and the knowledge of the asset.", 
                                          style = "color:black;text-align:justify"),
                                        p("Note that if only the structure of the system is known, Structural IM are used. Tha data about components reliabilities enable the usage of Reliability IM. 
                                          Lifetime IM includes lifetime of the components in the criticality analysis. Finally, if costs of components improvement or maintenance are significantly high,
                                          cost based IM should be applied. There are even IM that introduce uncertainty of data in components criticality analysis. The scope of this work includes the 
                                          Structural and Reliability IM.", style = "color:black;text-align:justify"),
                                        width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      tabsetPanel(
                                        tabPanel("Structural IM",
                                                 hr(),
                                                 h4(p(strong("Birnbaum"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("The first Structural IM was introduced by (Van Der Bost and Schoonakker, 2001) in order to analyze criticality of i-th components and it is commonly called Birnbaum
                                                     IM. Birnbaum IM of some component i is structural IM because it depends only on structure of the system and the reliability of the remaining system's components and 
                                                     does not depend on the actual reliability of component", em("i"),  "(Birnbaum, 1968).", style = "color:black;text-align:justify"),
                                                   p("If for a given component", em("i,"),  " the Birnbaum measure is large, it means that a small change in the reliability of the i-th component will result in a large 
                                                     change in the system reliability.", style = "color:black;text-align:justify"),
                                                   p("According to Thoft-Christensen and Rausand (2004), if system components are assumed independent, the Birnbaum measure of importance can be represented as exposed in 
                                                     the figure right.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="Birnbaum.JPG",width="600px",height="250px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Risk Reduction Worth - RRW"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="RRW.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("The RRW importance is a measure of the change in unreliability when an input variable, such as the unavailability of component, is set to zero-that is, 
                                                     by assuming that a component is 'perfect' (or its failure probability is zero) and thus eliminating any possibility of failure. This IM shows how much better the system 
                                                     can become as its components are improved. RRW can be computed as shows in the figure left.", style = "color:black;text-align:justify"),
                                                   p("In practice, this measure is use to identify elements of system that are the best candidates for improving system reliability.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Risk Achievement Worth - RAW"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("The RAW importance is the inverse of the RRW measure. In this method, the input variable (e.g., component unavailability) is set to one, and the effect of this change on 
                                                     system unreliability (unavailability) is measured. By setting component failure probability to one, RAW measures the increases in system failure probability assuming worst 
                                                     case of failing component. Therefore, RAW measure is as exposed in the figure right.", style = "color:black;text-align:justify"),
                                                   p("The risk increase measure is useful for identifying elements of the system that are the most crucial for making the system unreliable. Thus, components with high RAW 
                                                     importance are the one that will have the most impact, should their failure probability unexpectedly rise.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="RAW.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center")
                                        ),
                                        tabPanel("Reliability IM",
                                                 hr(),
                                                 h4(p(strong("Fussell-Vesely"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="FV.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Regarding the Reliability IM, Kiss (2011) proposes the Fussell-Vesely importance. This measure, introduced by (Fussell, 1975) is in the form as shown in the figure left.", 
                                                     style = "color:black;text-align:justify"),
                                                   p("In components with large Fussell-Vesely IM, it is important not to allow their long-term average probabilities to further increase. Accordingly, in an aging regime, 
                                                     Fussell-Vesely importance can be interpreted as the amount of allowed degradation of performance as a function of failure probability increases (Modarres et al., 1999).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Criticality - CR"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("Besides Fussell-Vesely IM based importance measures, CR importance measure is a reliability IM that is widely used in components criticality analysis. In other words, 
                                                     note that the Birnbaum measure does not consider the present of baseline performance (probability of success or failure of an element) so, it would be 
                                                     hard to use it for reliabiliity-informed decision making, since low-failure probability items are not necessarily the main candidates for any change 
                                                     (Makajic-Nikolic et al., 2018) and (Modarres et al., 1999). To remedy this shortcoming, an extended version of this measure may be used, called criticality importance 
                                                     and defined as shown in the figure right.", style = "color:black;text-align:justify"),
                                                   p("Note that the Birnbaum importance is correct for reliability of the individual components relative to the reliability of the whole system. Therefore, if the Birnbaum 
                                                     importance of a component is high, but the reliability of the component is low with respect to the reliability of the system, then criticality importance assigns a low 
                                                     importance to this component.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="CR.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"))
                                      ),
                                      fluidRow(width = 3),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Importance Measures Assessment for The DP System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(br(),DT::dataTableOutput("ImportanceM"), width = 11)
                                      )
                                      ), #Finish IM
                             tabPanel('Maintainability Analysis', icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Maintainability"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, tags$img(src="Mcurve.JPG",width="300px",height="250px", 
                                                                          style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                               column(
                                                 p("When a system fails to perform satisfactorily, repair is normally carried out to locate and correct the fault. The system is restored to
                                                 operational effectiveness by making an adjustment or by replacing a component.", style = "color:black;text-align:justify"),
                                                 p("Maintainability is defined as the probability that a failed system will be restored to specified conditions within a given period of time 
                                                   when maintenance is performed according to prescribed procedures and resources. In other words, maintainability is the probability of 
                                                   isolating and repairing a fault in a system within a given time (Pham, 2006).", style = "color:black;text-align:justify"),
                                                 p("To quantify repair times, let", em("T")," be the continuous random variable representing the time to repair a failed unit, having a 
                                                 probability density function (pdf) of repair", em("m(t).")," Then the cumulative distribution function is ",style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$P_r (T \\leq t) = M(t) = \\int_0^t m(t') \\, dt'$$",style="color:black;border:1px solid black;background-color:white"),
                                                 p("This equation is the probability that a repair will accomplishes within time", em("t.")," Therefore, the mean time to repair is given by ",
                                                   style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$MTTR = \\int_0^{\\infty} tm(t) \\, dt = \\int_0^{\\infty} [1-M(t)] \\, dt$$",style="color:black;border:1px solid black;background-color:white"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(width = 3),
                                      br(),
                                      fluidRow(column(width = 1),
                                               column(p("Particularly for the analysis of the DP System, the Monte Carlo Simulation was the maintainability technique used. The maintainability results for different operational 
                                               times are exposed to follow. Note that the inputs for the analysis are the failure and repair rates previously collected (see Data section of", em(" Functional Analysis)."), 
                                                        style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Maintainability Assessment for The DP System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 1),
                                               column(p("The next figures show the histograms of repair times obtained in the Monte Carlo Simulation and the respective distribution functions that best fit for periods from 1 to 
                                                        6 months, 9 months and 12 months. Basically, histograms are represented either by a Lognormal distribution, or by a Weibull. ", style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                      ),
                                      br(),
                                      fluidRow(column(width = 1), column(
                                        fluidRow(
                                          column(br(),tags$img(src="One_Two.JPG",width="750px",height="350px", 
                                                               style="display: block; margin-left: auto; margin-right: auto;"), 
                                                 br(), width=11)
                                        ), width = 11
                                      )
                                      ),
                                      br(),
                                      fluidRow(column(width = 1), column(
                                        fluidRow(
                                          column(br(),tags$img(src="Three_Four.JPG",width="750px",height="350px", 
                                                               style="display: block; margin-left: auto; margin-right: auto;"), 
                                                 br(), width=11)
                                        ), width = 11
                                      )
                                      ),
                                      br(),
                                      fluidRow(column(width = 1), column(
                                        fluidRow(
                                          column(br(),tags$img(src="Five_Six.JPG",width="750px",height="350px", 
                                                               style="display: block; margin-left: auto; margin-right: auto;"), 
                                                 br(), width=11)
                                        ), width = 11
                                      )
                                      ),
                                      br(),
                                      fluidRow(column(width = 1), column(
                                        fluidRow(
                                          column(br(),tags$img(src="Nine_Twelve.JPG",width="750px",height="350px", 
                                                               style="display: block; margin-left: auto; margin-right: auto;"), 
                                                 br(), width=11)
                                        ), width = 11
                                      )
                                      ),
                                      br(),
                                      fluidRow(column(width = 1),
                                               column(p("From the previous figures, it appears that, considering all established mission times, 
                                                        more than 85% of downtime lasts less than 40 hours and that about 70% of the downtimes are 
                                                        less than 4 hours. Specifically for a 3 month mission time, approximately 25% of system repair 
                                                        times are estimated to last less than 24 minutes. ", style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                      ),
                                      br(),
                                      br(),
                                      fluidRow()
                                      ), #Finish Maintainability Analysis
                             tabPanel("Availability Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Availability"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                        p("Reliability is a measure that requires system success for an entire mission time. No failures or repairs are allowed. Space missions and aircraft flights 
                                                   are examples of systems where failures or repairs are not allowed. Availability is a measure that allows for a system to repair when failure occurs (Pham, 
                                                   2006).", style = "color:black;text-align:justify"),
                                        p("The availability of a system is defined as the probability that a system is performing its required function at a given point in time or over a stated 
                                                   period of time when operated and maintained in a prescribed manner (Komal, Sharma and Kumar, 2010). Mathematically,", style = "color:black;text-align:justify"),
                                        withMathJax(),
                                        p("$$A = \\frac{(System up time)}{(System up time + System down time)} = \\frac{(MTBF)}{(MTBF + MTTR)}$$",style="color:black;border:1px solid black;background-color:white"),
                                        p("Where MTBF is Mean Time Between Failure and MTTR is Mean Time To Repair.", style = "color:black;text-align:justify"),
                                        p("The implication of this formula is that a high availability can be obtained either by increasing the MTBF, and hence the reliability, or improving the maintainability
                                                   by decreasing the MTTR (Leitch, 1995).", style = "color:black;text-align:justify"),
                                        p("Availability is a measure of success used primarily for repairable systems. For non-repairable systems, availability, ", em("A(t),")," equals reliability", em("R(t)."),
                                          " In reparable systems,", em("A(t),")," will be equal to or greater than", em("R(t)."), style = "color:black;text-align:justify"),
                                        width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Availability Assessment for The DP System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(br(),DT::dataTableOutput("AvailabilityTime"), width = 11)
                                      )
                                      ), #Finish Availability Analysis
                             tabPanel("References", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("References used in this work"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                        p("- ABS. (2013). Guide for Dynamic Positioning Systems. American Bureau of Shipping.", style = "color:black;text-align:justify"),
                                        p("- Aksu, S., & Turan, O. (2006). Reliability and availability of Pod propulsion systems. Quality and Reliability Engineering International, 22(1), 41-58. https://doi.org/10.1002/qre.747", style = "color:black;text-align:justify"),
                                        p("- A. Ghadiri, S. Mousavizadeh, and M. R. Haghifam, Reliability assessment of possible AC/DC distribution system configurations, 2014 Int. Conf. Probabilistic Methods Appl. to Power Syst. PMAPS 2014 - Conf. Proc., 2014.doi: 10.1109/PMAPS.2014.6960668.", style = "color:black;text-align:justify"),
                                        p("- Birnbaum, Z.W. (1968). On the importance of different components in a multicomponent system.", style = "color:black;text-align:justify"),
                                        p("- B. Parashar and G. Taneja, Reliability and profit evaluation of a PLC hot standby system based on a master-slave concept and two types of repair facilities, IEEE Trans. Reliab., vol. 56, 
                                          no. 3, pp. 534-539, 2007, doi: 10.1109/TR.2007.903151", style = "color:black;text-align:justify"),
                                        p("- Clavijo MV, Schleder AM, Droguett EL, Martins MR. RAM analysis of dynamic positioning system: An approach taking into account uncertainties and criticality equipment ratings. Proceedings of 
                                          the Institution of Mechanical Engineers, Part O: Journal of Risk and Reliability. October 2021. doi:10.1177/1748006X211051805", style = "color:black;text-align:justify"),
                                        p("- C. Himabindu and V. Sumalatha, Reliability analysis of Electronic Control Unit and Remote Unit used in Decoy Launcher Defence System, Int. J. Adv. Res. Ideas Innov. Technol., vol. 5, no. 3, 
                                          pp. 495-498, 2019.", style = "color:black;text-align:justify"),
                                        p("- EBrahimi, A. (2010). Effect analysis of RAMS parameter in design & operation of DP system in floating offshroe structure. Royal Institute of Technology, (October).", 
                                          style = "color:black;text-align:justify"),
                                        p("- EPRI. (2001). A Review of the Reliability of Electric Distribution System Components. Electric Power Research Institute.", style = "color:black;text-align:justify"),
                                        p("- Fussell, J. (1975). How to Hand-Calculate System Reliability and Safety Characteristics. IEEE Transactions on Reliability, 3, 169-174.", style = "color:black;text-align:justify"),
                                        p("- G. C. Gant, Reliability and Maintainability in a Turbine Governing System., Proc. Inst. Mech. Eng. Part A. Power Process Eng., vol. 201, no. A1, pp. 29-37, 1987, doi: 10.1243/PIME_PROC_1987_201_004_02.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Hartnett, R., Gross, K., Bovee, B., Nassar, S., Academy, U. S. C. G., Johnson, G., Associates, J. J. M. (2003). DGPS Accuracy Observations and Potential Data Channel Improvements. 59th Annual Meeting, 
                                          Institute of Navigation, 55-78.", style = "color:black;text-align:justify"),
                                        p("- IEEE. (2007). Design of Reliable Industrial and Commercial Power Systems. IEEE Std 493-2007. Institute of Electrical and Electronics Engineers.", style = "color:black;text-align:justify"),
                                        p("- Igba, J., Alemzadeh, K., Henningsen, K., & Durugbo, C. (2015). Performance assessment of wind turbine gearboxes using in-service data: Current approaches and future trends. Renewable & Sustainable Energy 
                                          Reviews, 50, 144-159.", style = "color:black;text-align:justify"),
                                        p("- Ince, A., Topuz, E., Panayirci, E. & Isik, C. (1998). COST ANALYSIS AND IMPLEMENTATION PLANNING . En PRINCIPLES OF INTEGRATED MARITIME SURVEILLANCE SYSTEMS (469-483). Turkey: SPRINGER SCIENCE.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Kiss, J. (2011). Matrix-Based Methods for Planning and. January 2011, 421-434.", style = "color:black;text-align:justify"),
                                        p("- Komal, Sharma, S. P., & Kumar, D. (2010). RAM analysis of repairable industrial systems utilizing uncertain data. Applied Soft Computing Journal, 10(4), 1208-1221.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Lehmkoster, J. (2014). Oil and Gas from the Sea.World Ocean Review 47.", style = "color:black;text-align:justify"),
                                        p("- Leitch, R. (1995). Reliability Analysis for Engineers: An introduction (Oxford University Press (ed.)).", style = "color:black;text-align:justify"),
                                        p("- L. Shi, C. Shan, X. Wu, and N. Zhao, Comparison of solid-state frequency converter and rotary frequency converter in 400Hz power system, 2011 Int. Conf. Electr. Mach. Syst. ICEMS 2011, pp. 2-6, 2011, 
                                          doi: 10.1109/ICEMS.2011.6073791.", style = "color:black;text-align:justify"),
                                        p("- Makajic-Nikolic, D., Vujosevic, M., Pavlovic, P., 2018. Importance measures in reliability and maintenance 7-16.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Modarres, M., Kaminskiy, M., & Krivtsov, V. (2009). Basic Reliability Mathematics: Review of Probability and Statistics. In CRC Press (Ed.), Reliability engineering and risk analysis (pp. 15-70).", 
                                          style = "color:black;text-align:justify"),
                                        p("- MTS. (1997). Reliability and Risk Analysis of DPS. Marine Technology Society. DYNAMIC POSITIONING CONFERENCE OCTOBER 1997", style = "color:black;text-align:justify"),
                                        p("- Navship. (2016). Stim Star Brasil, DPS-2 FMEA (E. C. Offshore (ed.)).", style = "color:black;text-align:justify"),
                                        p("- NIOT. (2015). Assessment of the reliability of the Indian tsunami buoy system. National Institute of Ocean Technology, 32(4), 255-270. https://doi.org/10.3723/ut.32.255", 
                                          style = "color:black;text-align:justify"),
                                        p("- NSWC. (2011). Handbook of Reliability Prediction Procedures for Mechanical Equipment.", style = "color:black;text-align:justify"),
                                        p("- OREDA. (2015). Offshore Reliability Data Handbook, 6th Edition. edited by SINTEF. Oslo.", style = "color:black;text-align:justify"),
                                        p("- Pham, H. (2006). System Software Reliability (Springer (ed.)).", style = "color:black;text-align:justify"),
                                        p("- Rappini, S., Pallaoro, A., & Heringer, M. (2003). Fundamentos de posicionamento dinamico.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Rasoulzadeh, V. (2015). Risk Assessment of Diesel Engine Failure in a Dynamic Positioning System. University of Stavanger.", style = "color:black;text-align:justify"),
                                        p("- RAYTHEON. (2010). Standard 22 M Gyro Compass System. ", style = "color:black;text-align:justify"),
                                        p("- SAFRAN. (2014). Attitude & Heading Reference System.", style = "color:black;text-align:justify"),
                                        p("- Salazar, J.C., Nejjari, F., Sarrate, R., Weber, P., Theilliol, D. (2016). Reliability importance measures for a health-aware control of drinking water networks. Conf. Control Fault-Tolerant Syst. 
                                          SysTol 2016-Novem, 572- 578.", style = "color:black;text-align:justify"),
                                        p("- Si, S., Cai, Z., Sun, S., Zhang, S. (2010). Integrated importance measures of multi-state systems under uncertainty. Comput. Ind. Eng. 59, 921-928.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Sudarmono, S.H., (2019). Application of Reliability, Availability and Maintainability analysis to Dynamic Positioning Systems used in offshore operations.", style = "color:black;text-align:justify"),
                                        p("- S. Grillo, E. Tironi, and G. Ubezio, Analysis of failure rates of ac and dc micro-grids, 14th IMEKO TC10 Work. Tech. Diagnostics 2016 New Perspect. Meas. Tools Tech. Syst. Reliab. Maintainab. Saf., 
                                          pp. 220-225, 2016.", style = "color:black;text-align:justify"),
                                        p("- S. Statkic, I. B. Jeftenic, M. Z. Bebic, Z. Milkic, and S. Jovic, Reliability assessment of the single motor drive of the belt conveyor on Drmno open-pit mine, Int. J. Electr. Power Energy Syst., 
                                          vol. 113, no. May, pp. 393-402, 2019, doi: 10.1016/j.ijepes.2019.05.062.", style = "color:black;text-align:justify"),
                                        p("- TMEIC. (2013). Variable Frequency Drives - a Comparison of VSI versus LCI Systems Introduction. Toshiba Mitsubishi-Electric Industrial Systems Corporation.", style = "color:black;text-align:justify"),
                                        p("- VAISALA. (2002). USER ' S GUIDE Combined Wind Sensor WMS301 and WMS302.", style = "color:black;text-align:justify"),
                                        p("- Van Der Borst, M., Schoonakker, H. (2001). An overview of PSA importance measures. Reliab. Eng. Syst. Saf. 72, 241-245.", style = "color:black;text-align:justify"),
                                        p("- Vishnu, C.R., and  Regikumar, V. (2016). Reliability Based Maintenance Strategy Selection in Process Plants: A Case Study. Procedia Technology 25 (Raerest): 1080-87.", 
                                          style = "color:black;text-align:justify"),
                                        
                                        
                                        
                                        
                                        
                                        
                                        width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br())) #Finish References
    
))
