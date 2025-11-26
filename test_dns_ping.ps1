param(
    [int]$Count = 10,
    [int]$Timeout = 1000,
    [switch]$Verbose
)

# Large list of 5000 DNS addresses (public DNS servers and common domains)
$DnsAddresses = @(
    # Major DNS Providers
    "8.8.8.8", "8.8.4.4", "1.1.1.1", "1.0.0.1", "9.9.9.9", "149.112.112.112",
    "208.67.222.222", "208.67.220.220", "64.6.64.6", "64.6.65.6",

    # Popular Websites
    "google.com", "youtube.com", "facebook.com", "twitter.com", "instagram.com",
    "linkedin.com", "reddit.com", "amazon.com", "ebay.com", "wikipedia.org",
    "netflix.com", "microsoft.com", "apple.com", "github.com", "stackoverflow.com",
    "cloudflare.com", "zoom.us", "dropbox.com", "spotify.com", "twitch.tv",

    # More DNS Servers
    "4.2.2.1", "4.2.2.2", "4.2.2.3", "4.2.2.4", "4.2.2.5", "4.2.2.6",
    "156.154.70.1", "156.154.71.1", "198.101.242.72", "23.253.163.53",
    "77.88.8.8", "77.88.8.1", "91.239.100.100", "89.233.43.71",
    "74.82.42.42", "109.69.8.51", "84.200.69.80", "84.200.70.40",

    # Country-specific DNS
    "168.95.1.1", "168.95.192.1", "203.80.96.10", "203.80.96.9",
    "202.134.0.155", "202.134.1.10", "210.220.163.82", "219.250.36.130",
    "180.76.76.76", "114.114.114.114", "119.29.29.29", "223.5.5.5", "223.6.6.6",

    # Tech Companies
    "oracle.com", "ibm.com", "cisco.com", "intel.com", "amd.com", "nvidia.com",
    "salesforce.com", "adobe.com", "vmware.com", "dell.com", "hp.com",
    "lenovo.com", "asus.com", "samsung.com", "lg.com", "sony.com",

    # Social Media & Communication
    "tiktok.com", "snapchat.com", "pinterest.com", "tumblr.com", "vimeo.com",
    "dailymotion.com", "discord.com", "telegram.org", "whatsapp.com", "skype.com",
    "slack.com", "teams.microsoft.com", "meet.google.com", "webex.com",

    # News & Media
    "cnn.com", "bbc.com", "nytimes.com", "theguardian.com", "reuters.com",
    "bloomberg.com", "forbes.com", "wsj.com", "washingtonpost.com", "npr.org",
    "foxnews.com", "cbsnews.com", "nbcnews.com", "abcnews.go.com",

    # E-commerce
    "aliexpress.com", "walmart.com", "target.com", "bestbuy.com", "etsy.com",
    "shopify.com", "alibaba.com", "rakuten.com", "booking.com", "expedia.com",
    "airbnb.com", "uber.com", "lyft.com", "doordash.com", "grubhub.com",

    # Education & Research
    "mit.edu", "stanford.edu", "harvard.edu", "berkeley.edu", "oxford.ac.uk",
    "cambridge.org", "arxiv.org", "researchgate.net", "academia.edu", "sciencedirect.com",
    "nature.com", "springer.com", "wiley.com", "jstor.org", "pubmed.gov",

    # Cloud & Hosting
    "aws.amazon.com", "azure.microsoft.com", "cloud.google.com", "digitalocean.com",
    "linode.com", "vultr.com", "heroku.com", "netlify.com", "vercel.com",
    "cloudways.com", "godaddy.com", "namecheap.com", "bluehost.com", "hostgator.com",

    # Developer Tools
    "gitlab.com", "bitbucket.org", "npmjs.com", "pypi.org", "maven.apache.org",
    "nuget.org", "packagist.org", "rubygems.org", "crates.io", "dockerhub.com",
    "kubernetes.io", "jenkins.io", "travis-ci.org", "circleci.com",

    # Security & Privacy
    "protonmail.com", "tutanota.com", "duckduckgo.com", "startpage.com",
    "qwant.com", "brave.com", "torproject.org", "nordvpn.com", "expressvpn.com",
    "malwarebytes.com", "kaspersky.com", "avast.com", "avg.com", "bitdefender.com",

    # Gaming
    "steam.com", "epicgames.com", "blizzard.com", "ea.com", "ubisoft.com",
    "nintendo.com", "playstation.com", "xbox.com", "roblox.com", "minecraft.net",
    "twitch.tv", "ign.com", "gamespot.com", "polygon.com", "kotaku.com",

    # Finance
    "paypal.com", "stripe.com", "square.com", "coinbase.com", "binance.com",
    "kraken.com", "blockchain.com", "chase.com", "bankofamerica.com", "wellsfargo.com",
    "citibank.com", "capitalone.com", "discover.com", "americanexpress.com",

    # Government
    "usa.gov", "irs.gov", "nih.gov", "nasa.gov", "whitehouse.gov",
    "sec.gov", "fda.gov", "cdc.gov", "state.gov", "justice.gov",
    "defense.gov", "va.gov", "usps.com", "census.gov", "weather.gov"
)

# Generate more addresses to reach 5000
$baseIndex = $DnsAddresses.Count
$generatedAddresses = @()

# Add more popular domains with subdomains
$popularDomains = @("google", "microsoft", "amazon", "apple", "meta", "yahoo", "baidu",
                     "yandex", "bing", "duckduckgo", "wordpress", "medium", "blogger")
$subdomains = @("www", "mail", "blog", "api", "dev", "test", "cdn", "static", "admin",
                "app", "mobile", "secure", "support", "help", "docs", "forum", "news",
                "shop", "store", "cloud", "drive", "photos", "maps", "translate", "analytics")

foreach ($domain in $popularDomains) {
    foreach ($subdomain in $subdomains) {
        $generatedAddresses += "$subdomain.$domain.com"
        if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
    }
    if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
}

# Add country code TLDs
$countries = @("uk", "de", "fr", "it", "es", "nl", "br", "ru", "cn", "jp", "kr", "in",
               "au", "ca", "mx", "ar", "za", "eg", "ng", "ke", "pl", "se", "no", "dk",
               "fi", "ch", "at", "be", "gr", "pt", "ie", "nz", "sg", "my", "th", "vn",
               "ph", "id", "tr", "sa", "ae", "il", "pk", "bd", "ua", "ro", "cz", "hu")
$servicePrefixes = @("mail", "smtp", "imap", "pop", "ftp", "www", "ns1", "ns2", "mx",
                      "web", "host", "server", "cloud", "vpn", "proxy", "gateway")

foreach ($country in $countries) {
    foreach ($prefix in $servicePrefixes) {
        $generatedAddresses += "$prefix.example.$country"
        if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
    }
    if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
}

# Add numbered subdomains for various services
$services = @("api", "cdn", "cache", "db", "app", "web", "srv", "node", "cluster", "pod")
for ($i = 1; $i -le 200; $i++) {
    foreach ($service in $services) {
        $generatedAddresses += "$service$i.example.com"
        if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
    }
    if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
}

# Add IP address ranges (Class A, B, C)
for ($a = 1; $a -le 255; $a += 5) {
    for ($b = 0; $b -le 255; $b += 50) {
        $generatedAddresses += "$a.$b.1.1"
        $generatedAddresses += "$a.$b.8.8"
        if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
    }
    if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
}

# Add more generic domains
for ($i = 1; $i -le 2000; $i++) {
    $generatedAddresses += "host$i.domain.com"
    $generatedAddresses += "server$i.net"
    $generatedAddresses += "site$i.org"
    if ($generatedAddresses.Count -ge (5000 - $baseIndex)) { break }
}

# Combine all addresses
$DnsAddresses += $generatedAddresses[0..([Math]::Min($generatedAddresses.Count, 5000 - $baseIndex) - 1)]

Write-Host "DNS Ping Tester - Total addresses in database: $($DnsAddresses.Count)" -ForegroundColor Cyan
Write-Host "Testing $Count addresses with $Timeout ms timeout`n" -ForegroundColor Yellow

$results = @()
$successCount = 0
$failCount = 0

# Randomly select addresses to test
$testAddresses = $DnsAddresses | Get-Random -Count ([Math]::Min($Count, $DnsAddresses.Count))

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
