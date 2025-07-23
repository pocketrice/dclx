for i in {1..17}
do
 #   wget -r -N -c -np "https://physionet.org/files/big-ideas-glycemic-wearable/1.1.2/0$i/HR_0$i.csv"
 cargo run -- HR_$i > HR_$i.log
     
done
