output$html_disclaimer <- renderText({
  email <- '<a href="mailto:stan.contact@oecd.org?subject=STI/EAS%20Indicators">stan.contact@oecd.org</a>'
  paste0('<br>',
         '<p>This website is hosted by the OECD Directorate for Science, Technology and Industry. It is responsible for the content of this site. The information published on this website have not been reviewed by the Organisation and do not necessarily represent the official views of the Organisation or of the governments of its member countries.</p>',
         '<p><center><strong>Login Information',
         '<div>Account: eas</div>',
         '<div>Password: eas</strong></center></div></p>',
         '<p>By making use of this website, you are explicitly declaring that you agree to the following general terms and conditions:</p>',
         '<p>Intellectual Property Rights',
         '<div>The information content of this site can be freely used as such for non-commercial purposes, on condition that OECD is identified as the original author. No rights may be derived from the content of these information, however. For uses other than that stated above, please contact ',
         email,
         '.</div></p>',
         '<p>Limitation of Liability',
         '<div>The directorate aims to achieve current and reliable indicators on this website. Despite the large degree of attention we put into the data compilation on this site, we cannot guarantee that the information is always correct or complete. If the information provided contains inaccuracies, the directorate will make every effort to rectify this as quickly as possible.</div>',
         '<div>The directorate cannot be held liable, however, for direct or indirect loss occurring as a result of the information on this site. Please notify the directorate of any inaccuracies or incomplete information: ',
         email,
         '. The content of this site can be amended, changed or supplemented at any time without notification or announcement.</div>',
         '<div>The directorate does not provide any guarantee of the proper operation of the website and cannot be held liable in any way for any poor operation or temporary nonavailability of the website, or for any form of loss, direct or indirect, which may ensue from access to or use of the website, including all losses, work interruptions, damage to programs or other data on the computer system, to equipment, software or other property of the user.</div></p>',
         '<br>',
         '<br>'
         )
})

output$html_territories <- renderText({
  ## require(XML)
  ## url <- "http://oecdcode.org/disclaimers/territories.html"
  ## htmlParse(url, encoding = "utf-8")
  paste0('<br>',
         '<p>Disclaimer (for the referring document)',
        '<div>This website and any map included herein are without prejudice to the status of or sovereignty over any territory, to the delimitation of international frontiers and boundaries and to the name of any territory, city or area.</div></p>',
         '<br>',
         '<br>'
        )
})

output$html_israel <- renderText({
  ## require(XML)
  ## url <- "http://oecdcode.org/disclaimers/israel.html"
  ## htmlParse(url)
  paste0('<br>',
         '<p>Information on the data for Israel appearing in OECD documents and publications',
        '<div>The statistical data for Israel are supplied by and under the responsibility of the relevant Israeli authorities. The use of such data by the OECD is without prejudice to the status of the Golan Heights, East Jerusalem and Israeli settlements in the West Bank under the terms of international law.</div></p>',
         '<br>',
         '<br>'
         )
})
