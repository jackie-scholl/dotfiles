function vpn --wraps='systemctl start openvpn-pritunl' --wraps='sudo systemctl restart openvpn-pritunl' --description 'alias vpn sudo systemctl restart openvpn-pritunl'
  sudo systemctl restart openvpn-pritunl $argv; 
end
