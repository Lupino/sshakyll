#!/usr/bin/env bash

mkdir -p /var/www

cat > /var/www/index.html <<__EOF__
<html><body>
<p>This Hakyll CMS site has been set up successfully, but no content has been published. Go click the "Publish" button! </p>
</body></html>
__EOF__

cp -a /source /var

/bin/sshakyll
