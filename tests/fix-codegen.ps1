$content = [System.IO.File]::ReadAllText('c2-codegen-v07.pbi')
$bad = 'PrintN(">>> EmitInt START op=FETCH nVar=" + Str(nVar) + " name=''' + "'" + ' + gVarMeta(nVar)\name + "' + "'" + ''")\r\n         Debug "V1.034.X: EmitInt START op=FETCH nVar=" + Str(nVar) + " name=''' + "'" + ' + gVarMeta(nVar)\name + "' + "'" + '''"'
$good = 'PrintN(">>> EmitInt START op=FETCH nVar=" + Str(nVar) + " name=''' + "'" + ' + gVarMeta(nVar)\name + "' + "'" + ''")'
$content = $content.Replace($bad, $good)
[System.IO.File]::WriteAllText('c2-codegen-v07.pbi', $content)
Write-Host "Fixed"
