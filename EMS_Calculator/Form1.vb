Public Class EMSCalculator
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim NUDs As New List(Of NumericUpDown) From
          {NUD1, NUD2, NUD3, NUD4, NUD5, NUD6, NUD7, NUD8, NUD9, NUD10, NUD11, NUD12, NUD13, NUD14, NUD15, NUD16, NUD17, NUD18, NUD19, NUD20,
           NUD21, NUD22, NUD23, NUD24, NUD25, NUD26, NUD27, NUD28, NUD29, NUD30, NUD31, NUD32, NUD33, NUD34, NUD35, NUD36, NUD37, NUD38, NUD39,
           NUD40, NUD41, NUD42, NUD43, NUD44, NUD45, NUD46, NUD47, NUD48, NUD49, NUD50, NUD51, NUD52, NUD53, NUD54, NUD55, NUD56, NUD57}

        For Each NUDIndex In NUDs
            NUDIndex.Value = 0
        Next
    End Sub
    Private Function GtNud(ByVal NumUpDown As NumericUpDown, ByVal Price As Integer)
        Dim var As Integer
        var = NumUpDown.Value * Price
        Return var
    End Function

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Dim TotalSum, MedkitPrice, BandagePrice As Integer
        If ChkBxQuantityDiscount.Checked = True Then
            If (NUD29.Value < 10) Then ''Obvazy
                BandagePrice = 400
                Label36.Text = "Obvaz - 400"
            ElseIf (NUD29.Value > 10) And (NUD29.Value < 50) Then
                BandagePrice = 375
                Label36.Text = "Obvaz - 375"
            ElseIf NUD29.Value = 10 Then
                BandagePrice = 375
                Label36.Text = "Obvaz - 375"
            ElseIf (NUD29.Value > 50) And (NUD29.Value < 100) Then
                BandagePrice = 350
                Label36.Text = "Obvaz - 350"
            ElseIf NUD29.Value = 50 Then
                BandagePrice = 350
                Label36.Text = "Obvaz - 350"
            ElseIf (NUD29.Value > 100) Then ''And (NUD29.Value < 150) Then
                BandagePrice = 300
                Label36.Text = "Obvaz - 300"
                ''   ElseIf NUD29.Value = 100 Then
                ''     BandagePrice = 300
            End If

            If (NUD30.Value < 10) Then ''Medkit
                MedkitPrice = 800
                Label35.Text = "Medkit - 800"
            ElseIf (NUD30.Value > 10) And (NUD30.Value < 50) Then
                MedkitPrice = 750
                Label35.Text = "Medkit - 750"
            ElseIf NUD30.Value = 10 Then
                MedkitPrice = 750
                Label35.Text = "Medkit - 750"
            ElseIf (NUD30.Value > 50) And (NUD30.Value < 100) Then
                MedkitPrice = 700
                Label35.Text = "Medkit - 700"
            ElseIf NUD30.Value = 50 Then
                MedkitPrice = 700
                Label35.Text = "Medkit - 700"
            ElseIf (NUD30.Value > 100) Then '' And (NUD30.Value < 150) Then
                MedkitPrice = 650
                Label35.Text = "Medkit - 650"
                ''ElseIf NUD30.Value = 100 Then
                ''  MedkitPrice = 650
            End If
        Else
            BandagePrice = 400
            MedkitPrice = 800
            Label36.Text = "Obvaz - 400"
            Label35.Text = "Medkit - 800"
        End If

        TotalSum =
            GtNud(NUD1, 900) +   ''Výjezdy v rámci města
            GtNud(NUD2, 1500) +  ''Výjezd Mimo město
            GtNud(NUD3, 5000) +  ''Vzlet Heli Mimo Město
            GtNud(NUD4, 2500) +  ''Vzlet Heli Město
            GtNud(NUD5, 5000) +  ''Falešný výjezd
            GtNud(NUD6, 15000) + ''Falešný vzlet
            GtNud(NUD7, 800) +   ''RTG SONO MRI CT
            GtNud(NUD8, 300) +   ''Osobní vyšetření
            GtNud(NUD9, 300) +   ''odřeniny a pořezání
            GtNud(NUD10, 300) +  ''prob z bezvědomí
            GtNud(NUD11, 700) +  ''popáleniny prvního stupně
            GtNud(NUD12, 700) +  ''Otřes mozku
            GtNud(NUD13, 10000) +  ''Psychotesty
            GtNud(NUD14, 1500) +  ''THC
            GtNud(NUD15, 2000) +  ''Metanfetamin
            GtNud(NUD16, 3000) +  ''Kokain
            GtNud(NUD17, 800) +  ''Alkohol
            GtNud(NUD18, 500) +  ''Normální pokoj
            GtNud(NUD19, 800) + ''JIP
            GtNud(NUD20, 1000) + ''VIP
            GtNud(NUD21, 500) + ''léky na bolest
            GtNud(NUD22, 300) + ''Adrenalin
            GtNud(NUD23, 400) + ''morphium
            GtNud(NUD24, 100) + ''Antibio
            GtNud(NUD25, 50) + ''Paralen
            GtNud(NUD26, 150) + ''předpis
            GtNud(NUD27, 500) + ''konopná mast
            GtNud(NUD28, 50) + _ ''na spaní
            GtNud(NUD29, BandagePrice) + ''Obvazy=================================
            GtNud(NUD30, MedkitPrice) + ''Medkit=================================
            GtNud(NUD31, 1500) + ''vytáhnutí kulky
            GtNud(NUD32, 600) + ''šití rány
            GtNud(NUD33, 1300) + ''popáleniny 2
            GtNud(NUD34, 900) + ''bodná rána
            GtNud(NUD35, 3500) + ''Vnitřní krvácení 
            GtNud(NUD36, 6000) + ''popáleniny 3
            GtNud(NUD37, 3000) + ''Fraktura lebky
            GtNud(NUD38, 800) + ''menší zlomenina
            GtNud(NUD39, 1000) + ''končetina
            GtNud(NUD40, 2000) + ''roztříštěná
            GtNud(NUD41, 2000) + ''žebra
            GtNud(NUD42, 3000) + ''otevřená 
            GtNud(NUD43, 1200) + ''vykloubení
            GtNud(NUD44, 1500) + ''napravení kosti
            GtNud(NUD45, 100) + ''obvaz
            GtNud(NUD46, 150) + ''dezinfekce
            GtNud(NUD47, 400) + ''rajský plyn
            GtNud(NUD48, 20) + ''náplast
            GtNud(NUD49, 500) + ''protilátky
            GtNud(NUD50, 1200) + ''protijed
            GtNud(NUD51, 250) + ''analgetika
            GtNud(NUD52, 200) + ''berle
            GtNud(NUD53, 100) + ''sádra/dlaha
            GtNud(NUD54, 200) + ''fyziologický roztok
            GtNud(NUD55, 250) + '' plasmylyte
            GtNud(NUD56, 450) + '' infuze krve
            GtNud(NUD57, 350) '' sedativa

        TxBxTotalSum.Text = TotalSum & Chr(32) & Chr(36)

    End Sub

End Class
