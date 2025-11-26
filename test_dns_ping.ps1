param(
    [int]$Count = 10,
    [int]$Timeout = 1000,
    [switch]$Verbose,
    [int]$PoolSize = 5000
)

# Function to generate random IP address
function Get-RandomIPAddress {
    $octets = @()

    # First octet: avoid 0, 127, 224-255 (reserved ranges)
    $octets += Get-Random -Minimum 1 -Maximum 224 | Where-Object { $_ -ne 127 }

    # Second and third octets: any value
    $octets += Get-Random -Minimum 0 -Maximum 256
    $octets += Get-Random -Minimum 0 -Maximum 256

    # Fourth octet: avoid 0 and 255
    $octets += Get-Random -Minimum 1 -Maximum 255

    return ($octets -join '.')
}

# Function to generate random domain name
function Get-RandomDomain {
    $tlds = @('com', 'net', 'org', 'io', 'dev', 'app', 'cloud', 'info', 'biz', 'tech')
    $prefixes = @('api', 'cdn', 'mail', 'www', 'app', 'web', 'cloud', 'data', 'host', 'server',
                  'node', 'db', 'cache', 'proxy', 'vpn', 'dns', 'ns1', 'ns2', 'mx', 'smtp')
    $domains = @('example', 'test', 'demo', 'sample', 'service', 'system', 'network', 'platform',
                 'digital', 'tech', 'cloud', 'data', 'media', 'global', 'enterprise')

    $useSubdomain = (Get-Random -Minimum 0 -Maximum 2) -eq 1

    if ($useSubdomain) {
        $prefix = $prefixes | Get-Random
        $domain = $domains | Get-Random
        $tld = $tlds | Get-Random
        return "$prefix.$domain.$tld"
    }
    else {
        $domain = $domains | Get-Random
        $number = Get-Random -Minimum 1 -Maximum 9999
        $tld = $tlds | Get-Random
        return "$domain$number.$tld"
    }
}

Write-Host "DNS Ping Tester - Generating $PoolSize random addresses..." -ForegroundColor Cyan

# Generate random pool of addresses (mix of IPs and domains)
$DnsAddresses = @()

# Add some known working DNS servers for reference (10%)
$knownDns = @(
    "8.8.8.8", "8.8.4.4", "1.1.1.1", "1.0.0.1", "9.9.9.9", "149.112.112.112",
    "208.67.222.222", "208.67.220.220", "64.6.64.6", "64.6.65.6",
    "4.2.2.1", "4.2.2.2", "156.154.70.1", "156.154.71.1"
)

# Add some known working domains (5%)
$knownDomains = @(
    "google.com", "cloudflare.com", "microsoft.com", "amazon.com", "github.com",
    "stackoverflow.com", "wikipedia.org", "youtube.com", "reddit.com", "twitter.com"
)

$knownCount = [Math]::Floor($PoolSize * 0.15)
$randomCount = $PoolSize - $knownCount

# Add known addresses
for ($i = 0; $i -lt $knownCount; $i++) {
    if ($i % 3 -eq 0 -and $knownDomains.Count -gt 0) {
        $DnsAddresses += $knownDomains | Get-Random
    }
    else {
        $DnsAddresses += $knownDns | Get-Random
    }
}

# Generate random addresses (70% IPs, 30% domains)
for ($i = 0; $i -lt $randomCount; $i++) {
    if ((Get-Random -Minimum 0 -Maximum 10) -lt 7) {
        $DnsAddresses += Get-RandomIPAddress
    }
    else {
        $DnsAddresses += Get-RandomDomain
    }

    if (($i % 500) -eq 0) {
        Write-Progress -Activity "Generating Random Addresses" -Status "Generated $i of $randomCount" -PercentComplete (($i / $randomCount) * 100)
    }
}

Write-Progress -Activity "Generating Random Addresses" -Completed

# Shuffle the array to randomize order
$DnsAddresses = $DnsAddresses | Sort-Object { Get-Random }

Write-Host "Generated $($DnsAddresses.Count) random addresses" -ForegroundColor Green
Write-Host "Testing $Count addresses with $Timeout ms timeout`n" -ForegroundColor Yellow

$results = @()
$successCount = 0
$failCount = 0

# Select first N addresses to test (already randomized)
$testAddresses = $DnsAddresses[0..([Math]::Min($Count, $DnsAddresses.Count) - 1)]

$i = 0
foreach ($address in $testAddresses) {
    $i++
    Write-Progress -Activity "Testing DNS Addresses" -Status "Testing $address ($i of $Count)" -PercentComplete (($i / $Count) * 100)

    try {
        $ping = Test-Connection -ComputerName $address -Count 1 -TimeoutSeconds ($Timeout / 1000) -ErrorAction Stop
        $status = "SUCCESS"
        $responseTime = $ping.Latency
        $successCount++
        $color = "Green"
    }
    catch {
        $status = "FAILED"
        $responseTime = "N/A"
        $failCount++
        $color = "Red"
    }

    $result = [PSCustomObject]@{
        Address = $address
        Status = $status
        ResponseTime = $responseTime
        Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    }

    $results += $result

    if ($Verbose) {
        Write-Host "[$i/$Count] $address - $status" -ForegroundColor $color
        if ($responseTime -ne "N/A") {
            Write-Host "  Response Time: $responseTime ms" -ForegroundColor Gray
        }
    }
}

Write-Progress -Activity "Testing DNS Addresses" -Completed

# Display summary
Write-Host "`n========== SUMMARY ==========" -ForegroundColor Cyan
Write-Host "Total Tested: $Count" -ForegroundColor White
Write-Host "Successful:   $successCount" -ForegroundColor Green
Write-Host "Failed:       $failCount" -ForegroundColor Red
Write-Host "Success Rate: $([Math]::Round(($successCount / $Count) * 100, 2))%" -ForegroundColor Yellow
Write-Host "============================`n" -ForegroundColor Cyan

# Display results table
$results | Format-Table -AutoSize

# Calculate average response time for successful pings
$avgResponseTime = ($results | Where-Object { $_.ResponseTime -ne "N/A" } | Measure-Object -Property ResponseTime -Average).Average
if ($avgResponseTime) {
    Write-Host "Average Response Time: $([Math]::Round($avgResponseTime, 2)) ms" -ForegroundColor Magenta
}

# Export results to CSV
$csvFile = "dns_ping_results_$(Get-Date -Format 'yyyyMMdd_HHmmss').csv"
$results | Export-Csv -Path $csvFile -NoTypeInformation
Write-Host "`nResults exported to: $csvFile" -ForegroundColor Green
