#!/bin/sh

echo "Enter client 2 IP: "
read client2_ip

while true; do
	filename="/tmp/__telraam_dump_$(date +%FT%T).sql.gz"

	echo "Dumping telraam..."
	sudo -u postgres pg_dump -d telraam --inserts --rows-per-insert=2000 -Ft | gzip > "${filename}"

	echo "Copying dump to client 2 (${filename})..."
	scp "${filename}" "zeus@${client2_ip}:/home/zeus/telraam_dumps"

	echo "Removing local dump..."
	sudo rm -f "${filename}"

	echo "Done"
	echo ""
	sleep 300
done
