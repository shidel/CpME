#!/bin/bash

# I don't want the command line build to increment the build number automatically
# Backup the LPI file. After the build, restore those files to reset the build number.

function build_default () {

	local app="${1}"
	local cpu=$(uname -m)
	local os=$(uname -s)

	# [[ -f "${app}".app ]] && rm "${app}".app
	# [[ -f "${app}" ]] && rm "${app}"
	# [[ "${os}" == "Darwin" ]] && os=macOS
	# cp ${app}.ver ${app}.lpi
	# [[ -e ${app} ]] && rm ${app}
	# lazbuild -B -r ${app}.lpr
	if [[ -e ${app}.app ]] then
		mkdir -p ../binaries/${cpu}/${os}
		cp -fav ${app}.app ../binaries/${cpu}/${app}.app
	elif [[ -e ${app} ]] ; then
		mkdir -p ../binaries/${cpu}/${os}
		cp ${app} ../binaries/${cpu}/${os}/
	else
		exit 1
	fi

}

function build_app () {

	local app="${1}"

	cp ${app}.lpi ${app}.ver
	build_default ${app}
	cp ${app}.ver ${app}.lpi
	rm ${app}.ver

}

function lines () {
	m=$(wc -l *.pas *.lpr | tail -n 1 | cut -d 't' -f 1)
	# s=$(wc -l example/*.conf example/*.ini *.nls *.oss *.url | tail -n 1 | cut -d 't' -f 1)
	# t=$(wc -l example/template/*.css example/template/*.html | tail -n 1 | cut -d 't' -f 1)
	# c=$(( ${m} + ${s} + ${t} ))

	echo "Program sources: "${m}
	# echo "Settings files:  "${s}
	# echo "Template files:  "${t}
	# echo "Total lines:      "${c}
	echo
}

build_app CpME

lines

