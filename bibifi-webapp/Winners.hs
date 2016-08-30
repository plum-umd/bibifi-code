module Winners where

import Import

fall2014 :: Widget
fall2014 = do
    meh <- newIdent
    error309 <- newIdent
    ssrg_vt <- newIdent
    woopwoop <- newIdent
    winners <- newIdent
    toWidget [lucius|
            ##{meh} {
                width: 585px;
                height: 390px;
            }
            ##{error309} {
                width: 585px;
                height: 390px;
            }
            ##{ssrg_vt} {
                width: 585px;
                height: 390px;
            }
            ##{winners} {
                width: 585px;
                padding: 20px 0px;
            }
        |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center" id="winners">
                    Fall 2014 Winners
                <div>
                    <p id="#{winners}" class="center-block">
                        The Fall 2014 contest has ended! 
                        Congratulations to the winners!
                        Team <em>meh</em> took first place as a builder. 
                        The second place builder team was <em>Error309</em>. 
                        The first place breaker team was <em>SSRG_VT</em>. 
                        Team <em>woopwoop</em> won second place as a breaker. 
                        Check out the <a href="@{SpecificScoreboardR "fall2014"}">scoreboard</a> for final scores. 
        
        <div class="row">
            <div .text-center>
                <img id="#{meh}" src="@{StaticR img_fall2014_meh_jpeg}" .img-rounded>
                <p>
                    <strong>
                        First place builders, team <em>meh</em>.
        <div class="row">
            <div .text-center>
                <img id="#{error309}" src="@{StaticR img_fall2014_error309_jpg}" .img-rounded>
                <p>
                    <strong>
                        Second place builders, team <em>Error309</em>.
        <div class="row">
            <div .text-center>
                <img id="#{ssrg_vt}" src="@{StaticR img_fall2014_ssrg_vt_jpg}" .img-rounded>
                <p>
                    <strong>
                        First place breakers, team <em>SSRG_VT</em>.
        <div class="row">
            <div .text-center>
                <img id="#{woopwoop}" src="@{StaticR img_fall2014_woopwoop_jpg}" .img-rounded>
                <p>
                    <strong>
                        Second place breakers, team <em>woopwoop</em>.
    |]

spring2015 :: Widget
spring2015 = do
    javathehut1 <- newIdent
    javathehut2 <- newIdent
    javathehut3 <- newIdent
    javathehut4 <- newIdent
    seada1 <- newIdent
    seada2 <- newIdent
    seada3 <- newIdent
    seada4 <- newIdent
    gruyere1 <- newIdent
    gruyere2 <- newIdent
    blackhorse1 <- newIdent
    blackhorse2 <- newIdent
    tosca1 <- newIdent
    winners <- newIdent
    toWidget [lucius|
            ##{javathehut1} {
                width: 300px;
                height: 327px;
                margin: 10px;
            }
            ##{javathehut2} {
                width: 300px;
                height: 288px;
                margin: 10px;
            }
            ##{javathehut3} {
                width: 300px;
                height: 403px; 
                margin: 10px;
            }
            ##{javathehut4} {
                margin: 10px;
            }
            ##{seada2} {
                width: 300px;
                height: 231px; 
                margin: 10px;
            }
            ##{seada1} {
                width: 300px;
                height: 360px; 
                margin: 10px;
            }
            ##{seada3} {
                width: 300px;
                height: 300px; 
                margin: 10px;
            }
            ##{seada4} {
                width: 300px;
                height: 225px; 
                margin: 10px;
            }
            ##{gruyere1} {
                width: 250px;
                height: 333px; 
                margin: 10px;
            }
            ##{gruyere2} {
                margin: 10px;
            }
            ##{blackhorse1} {
                width: 300px;
                height: 329px; 
                margin: 10px;
            }
            ##{blackhorse2} {
                width: 300px;
                height: 323px; 
                margin: 10px;
            }
            ##{tosca1} {
                width: 400px;
                height: 300px; 
                margin: 10px;
            }
            ##{winners} {
                width: 585px;
                padding: 20px 0px;
            }
        |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center" id="winners">
                    Spring 2015 Winners
                <div>
                    <p id="#{winners}" class="center-block">
                        The Spring 2015 contest has ended! 
                        Congratulations to the winners!
                        Team <em>JavaTheHut</em> took first place as a builder. 
                        The second place builder team was <em>SEADA</em>. 
                        The third place builder team was <em>Gruyere</em>. 
                        The first place breaker team was <em>Black_Horse</em>. 
                        Team <em>Tosca</em> won second place as a breaker. 
                        The third place break team was <em>newbie1</em>. 
                        Check out the <a href="@{SpecificScoreboardR "spring2015coursera"}">scoreboard</a> for final scores. 
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{javathehut1}" src="@{StaticR img_spring2015_javathehut1_jpg}" .img-rounded>
                <img id="#{javathehut2}" src="@{StaticR img_spring2015_javathehut2_jpeg}" .img-rounded>
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{javathehut4}" src="@{StaticR img_spring2015_javathehut4_png}" .img-rounded>
                <img id="#{javathehut3}" src="@{StaticR img_spring2015_javathehut3_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        First place builders, team <em>JavaTheHut</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{seada1}" src="@{StaticR img_spring2015_seada1_jpg}" .img-rounded>
                <img id="#{seada3}" src="@{StaticR img_spring2015_seada3_jpg}" .img-rounded>
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{seada2}" src="@{StaticR img_spring2015_seada2_jpg}" .img-rounded>
                <img id="#{seada4}" src="@{StaticR img_spring2015_seada4_JPG}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place builders, team <em>SEADA</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{gruyere1}" src="@{StaticR img_spring2015_gruyere1_jpg}" .img-rounded>
                <img id="#{gruyere2}" src="@{StaticR img_spring2015_gruyere2_png}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Third place builders, team <em>Gruyere</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{blackhorse1}" src="@{StaticR img_spring2015_blackhorse1_png}" .img-rounded>
                <img id="#{blackhorse2}" src="@{StaticR img_spring2015_blackhorse2_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        First place breakers, team <em>Black_Horse</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{tosca1}" src="@{StaticR img_spring2015_tosca_JPG}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place breakers, team <em>Tosca</em>.
    |]

fall2015 :: Widget
fall2015 = do
    winners <- newIdent
    woopwoop <- newIdent
    boilers <- newIdent
    archonauts <- newIdent
    toWidget [lucius|
        ##{winners} {
            width: 585px;
            padding: 20px 0px;
        }
        ##{boilers} {
            width: 500px;
            height: 332px;
            margin-top: 10px;
        }
        ##{archonauts} {
            width: 300px;
            height: 400px;
            margin-top: 10px;
        }
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center" id="winners">
                    Fall 2015 Winners
                <div>
                    <p id="#{winners}" class="center-block">
                        The Fall 2015 contest has ended! 
                        Congratulations to the winners!
                        Team <em>woopwoop</em> took first place as a builder. 
                        The second place builder and first place breaker was team <em>b01lers</em>. 
                        Team <em>Archonauts</em> won second place as a breaker. 
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{woopwoop}" src="@{StaticR img_fall2015_woopwoop_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        First place builders, team <em>woopwoop</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{boilers}" src="@{StaticR img_fall2015_b01lers_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place builders and first place breakers, team <em>b01lers</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-8 .text-center>
                <img id="#{archonauts}" src="@{StaticR img_fall2015_Archonauts_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place breakers, team <em>Archonauts</em>.
    |]

fall2015coursera :: Widget
fall2015coursera = do
    teamBlue1 <- newIdent
    teamBlue2 <- newIdent
    teamBlue3 <- newIdent
    teamBlue4 <- newIdent
    teamBlue5 <- newIdent
    dropTable <- newIdent
    flagbit1 <- newIdent
    flagbit2 <- newIdent
    flagbit3 <- newIdent
    flagbit4 <- newIdent
    flagbit5 <- newIdent
    erlang <- newIdent
    winners <- newIdent
    toWidget [lucius|
        ##{winners} {
            width: 585px;
            padding: 20px 0px;
        }

        ##{teamBlue1} {
            width: 300px;
            height: 300px;
        }
        ##{teamBlue2} {
            width: 240px;
            height: 400px;
            margin-top: 15px;
        }
        ##{teamBlue3} {
            margin: 19px 0px;
            width: 300px;
            height: 262px;
        }
        ##{teamBlue4} {
            margin: 28px 0px;
            width: 300px;
            height: 243px;
        }
        ##{teamBlue5} {
            width: 300px;
            height: 300px;
        }
        ##{dropTable} {
            margin-top: 15px;
        }
        ##{flagbit1} {
            height: 386px;
            width: 290px;
            margin: 10px 0px;
        }
        ##{flagbit2} {
            height: 386px;
            width: 163px;
            margin: 10px 0px;
        }
        ##{flagbit3} {
            height: 360px;
            width: 280px;
            margin: 10px 0px;
        }
        ##{flagbit4} {
            height: 360px;
            width: 290px;
            margin: 10px 0px;
        }
        ##{flagbit5} {
            height: 330px;
            width: 280px;
            margin-top: 10px;
        }
        ##{erlang} {
            margin-top: 10px;
        }
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center" id="winners">
                    Fall 2015 Coursera Winners
                <div>
                    <p id="#{winners}" class="center-block">
                        The Fall 2015 coursera contest has ended! 
                        Congratulations to the winners!
                        Team <em>Team_Blue</em> took first place as a builder. 
                        The second place builder team was <em>DropTable</em>. 
                        The first place breaker team was <em>Flagbit</em>. 
                        Team <em>Erlang</em> won second place as a breaker. 
        <div class="row">
            <div .col-md-offset-2 .col-md-4 .text-center>
                <img id="#{teamBlue1}" src="@{StaticR img_fall2015_team_blue1_jpg}" .img-rounded>
            <div .col-md-4 .text-center>
                <img id="#{teamBlue3}" src="@{StaticR img_fall2015_team_blue3_jpg}" .img-rounded>
        <div class="row">
            <div .col-md-offset-2 .col-md-4 .text-center>
                <img id="#{teamBlue4}" src="@{StaticR img_fall2015_team_blue4_jpg}" .img-rounded>
            <div .col-md-4 .text-center>
                <img id="#{teamBlue5}" src="@{StaticR img_fall2015_team_blue5_JPG}" .img-rounded>
        <div class="row">
            <div .col-md-offset-4 .col-md-4 .text-center>
                <img id="#{teamBlue2}" src="@{StaticR img_fall2015_team_blue2_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        First place builders, team <em>Team_Blue</em>.
        <div class="row">
            <div .col-md-offset-4 .col-md-4 .text-center>
                <img id="#{dropTable}" src="@{StaticR img_fall2015_drop_table_jpeg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place builders, team <em>DropTable</em>.
        <div class="row">
            <div .col-md-offset-2 .col-md-4 .text-center>
                <img id="#{flagbit1}" src="@{StaticR img_fall2015_flagbit1_jpg}" .img-rounded>
            <div .col-md-4 .text-center>
                <img id="#{flagbit2}" src="@{StaticR img_fall2015_flagbit2_png}" .img-rounded>
        <div class="row">
            <div .col-md-offset-2 .col-md-4 .text-center>
                <img id="#{flagbit3}" src="@{StaticR img_fall2015_flagbit3_jpg}" .img-rounded>
            <div .col-md-4 .text-center>
                <img id="#{flagbit4}" src="@{StaticR img_fall2015_flagbit4_jpg}" .img-rounded>
        <div class="row">
            <div .col-md-offset-4 .col-md-4 .text-center>
                <img id="#{flagbit5}" src="@{StaticR img_fall2015_flagbit5_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        First place breakers, team <em>Flagbit</em>.
        <div class="row">
            <div .col-md-offset-4 .col-md-4 .text-center>
                <img id="#{erlang}" src="@{StaticR img_fall2015_erlang_jpg}" .img-rounded>
        <div class="row">
            <div .text-center>
                <p>
                    <strong>
                        Second place breakers, team <em>Erlang</em>.
    |]
