module Sponsors where

import Import

booz :: Widget
booz = do
    c <- newIdent
    toWidget [lucius|
        ##{c} {
            width: 100%;
        }
    |]
    [whamlet'|
        <a href="http://www.boozallen.com/">
            <img id="#{c}" src="@{StaticR img_booz_png}">
    |]

cigital :: Widget
cigital = do
    cigital <- newIdent
    toWidget [lucius|
        ##{cigital} {
            margin: 40px 0px 30px;
        }
    |]
    [whamlet'|
        <a href="http://www.cigital.com/">
            <img id="#{cigital}" src="@{StaticR img_cigital_png}">
    |]

leidos :: Widget
leidos = do
    ident <- newIdent
    toWidget [lucius|
        ##{ident} {
            width: 370px;
            height: 90px;
            margin: 29px 0px;
        }
    |]
    [whamlet'|
        <a href="https://www.leidos.com/">
            <img id="#{ident}" src="@{StaticR img_leidos_png}">
    |]

lockheed :: Widget
lockheed = do
    ident <- newIdent
    toWidget [lucius|
        ##{ident} {
            width: 370px;
            height: 105px;
        }
    |]
    [whamlet'|
        <a href="http://www.lockheedmartin.com/">
            <img id="#{ident}" src="@{StaticR img_lockheed_jpg}">
    |]

synopsys :: Widget
synopsys = do
    ident <- newIdent
    toWidget [lucius|
        ##{ident} {
            width: 350px;
            height: 78px;
            margin: 25px 0px 13px;
        }
    |]
    [whamlet'|
        <a href="http://www.synopsys.com/home.aspx">
            <img id="#{ident}" src="@{StaticR img_synopsys_png}">
    |]

galois :: Widget
galois = do
    ident <- newIdent
    toWidget [lucius|
        ##{ident} {
            margin: 24px 0px 12px;
        }
    |]
    [whamlet'|
        <a href="http://galois.com/">
            <img id="#{ident}" src="@{StaticR img_galois_png}">
    |]

accenture :: Widget
accenture = do
    ident <- newIdent
    toWidget [lucius|
        ##{ident} {
            width: 370px;
            height: 148px;
        }
    |]
    [whamlet'|
        <a href="https://www.accenture.com/">
            <img id="#{ident}" src="@{StaticR img_accenture_jpg}">
    |]

astech :: Widget
astech = do
    astech <- newIdent
    toWidget [lucius|
        ##{astech} {
            margin: 26px 0px 21px;
        }
    |]
    [whamlet'|
        <a href="https://www.astechconsulting.com/">
            <img id="#{astech}" src="@{StaticR img_astech_png}">
    |]

    -- patriot <- newIdent
    -- cyberpoint <- newIdent
    -- att <- newIdent
    -- suprtek <- newIdent
    -- astech <- newIdent
    -- galois <- newIdent
    -- isec <- newIdent
    --     ##{cyberpoint} {
    --         margin: 20px 0px;
    --     }
    --     ##{isec} {
    --         width: 300px;
    --         height: 300px;
    --         margin: 5px 0px;
    --     }
    --     ##{galois} {
    --         margin: 12px 0px;
    --     }
    --     ##{astech} {
    --         margin: 8px 0px;
    --     }
    --     ##{suprtek} {
    --         width: 442px;
    --         height: 349px;
    --     }
    --     ##{att} {
    --         width: 315px;
    --         height: 182px;
    --     }
    --     ##{patriot} {
    --         width: 250px;
    --         height: 88px;
    --         margin: 16px 0px 25px;
    --     }
    --         <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
    --             ^{Sponsors.cigital}
    --         <div class="sponsors col-sm-6 col-lg-5 text-center">
    --             <a href="http://patriot-tech.com/">
    --                 <img id="#{patriot}" src="@{StaticR img_patriot_png}">
    --             <a href="http://galois.com/">
    --                 <img id="#{galois}" src="@{StaticR img_galois_png}">
    --             <a href="https://www.astechconsulting.com/">
    --                 <img id="#{astech}" src="@{StaticR img_astech_png}">
    --             <a href="https://www.isecpartners.com/">
    --                 <img id="#{isec}" src="@{StaticR img_iSec_jpg}">
    --             <a href="http://www.suprtek.com/">
    --                 <img id="#{suprtek}" src="@{StaticR img_suprtek_png}">
    --             <a href="http://www.att.com/">
    --                 <img id="#{att}" src="@{StaticR img_att_jpg}">
    --             <a href="http://cyberpointllc.com/">
    --                 <img id="#{cyberpoint}" src="@{StaticR img_cyberpoint_png}">*/
