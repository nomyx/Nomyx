
date=$(date '+%Y-%m-%d')
version=0.7.2

cd /home/cdupont/tmp
tar -czvf "Nomyx-dep-$version.tar.gz" Nomyx/
rm -rf Nomyx
tar -xzvf  ~/Nomyx-$version.tar.gz
mv Nomyx-$version Nomyx

