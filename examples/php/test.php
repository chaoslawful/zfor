<?php
$ntimes=100000;
$ips=array();
$start=microtime(true);
for($i=0;$i<$ntimes;++$i) {
	$ip=zfor_gethostbyname("balance.test.com");
	$ips[$ip]++;
}
$end=microtime(true);
echo "Time per call: ",($end-$start)/$ntimes,"\n";
var_dump($ips);
?>
