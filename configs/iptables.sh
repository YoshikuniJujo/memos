sudo iptables -F INPUT
sudo iptables -F OUTPUT
sudo iptables -F FORWARD

sudo iptables -P INPUT DROP
sudo iptables -P OUTPUT DROP
sudo iptables -P FORWARD DROP

sudo iptables -A INPUT -i lo -j ACCEPT
sudo iptables -A OUTPUT -o lo -j ACCEPT

sudo iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
sudo iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

sudo iptables -A OUTPUT -p udp --dport 53 -j ACCEPT
sudo iptables -A INPUT -p udp --sport 53 -j ACCEPT

sudo iptables -A INPUT -p icmp --icmp-type 0 -j ACCEPT
sudo iptables -A INPUT -p icmp --icmp-type 8 -j ACCEPT
sudo iptables -A OUTPUT -p icmp --icmp-type 0 -j ACCEPT
sudo iptables -A OUTPUT -p icmp --icmp-type 8 -j ACCEPT
# sudo iptables -A OUTPUT -p icmp --icmp-type 0 -m owner --uid-owner tatsuya -j ACCEPT
# sudo iptables -A OUTPUT -p icmp --icmp-type 8 -m owner --uid-owner tatsuya -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner portage -p tcp --dport 80 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 80 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner root -p tcp --dport 80 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 80 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner root -p tcp --dport 443 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 443 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp --dport 443 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 443 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner portage -p tcp --dport 443 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d github.com --dport 443 -j ACCEPT
sudo iptables -A INPUT -p tcp -s github.com --sport 443 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d github.githubassets.com --dport 443 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s github.githubassets.com --sport 443 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d www.google-analytics.com --dport 443 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s www.google-analytics.com --sport 443 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d www.google-analytics.com --dport 80 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s www.google-analytics.com --sport 80 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d customer-stories-feed.github.com --dport 443 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s customer-stories-feed.github.com --sport 443 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d scratch.mit.edu --dport 443 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s scratch.mit.edu --sport 443 -j ACCEPT

# sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp -d fast.wistia.com --dport 443 -j ACCEPT
# sudo iptables -A INPUT -p tcp -s fast.wistia.com --sport 443 -j ACCEPT

sudo ip6tables -F INPUT
sudo ip6tables -F OUTPUT
sudo ip6tables -F FORWARD

sudo ip6tables -P INPUT DROP
sudo ip6tables -P OUTPUT DROP
sudo ip6tables -P FORWARD DROP

sudo iptables -A OUTPUT -m owner --uid-owner root -p udp --dport 123 -j ACCEPT
sudo iptables -A INPUT -p udp --sport 123 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner tatsuya -p tcp --dport 22 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 22 -j ACCEPT

sudo iptables -A OUTPUT -m owner --uid-owner root -p tcp --dport 873 -j ACCEPT
sudo iptables -A INPUT -p tcp --sport 873 -j ACCEPT
