#!/bin/bash

echo "This may take a while."

echo "Add all addresses from futurice to Archive lbdb."
find ~/.mail/futurice/Archive/cur | sed s/\ /\\"\ "/g | grep -v /./ | while read messge
   do
      if [ -f "$messge" ]
      then
            cat "$messge" | lbdb-fetchaddr -c 'utf-8'
      fi
   done

echo "Remove duplicates."
sort ~/.lbdb/m_inmail.utf-8 | uniq -u > /tmp/lbdb.tmp
mv /tmp/lbdb.tmp ~/.lbdb/m_inmail.utf-8
rm ~/.lbdb/m_inmail.utf-8.dirty

echo "Done."
