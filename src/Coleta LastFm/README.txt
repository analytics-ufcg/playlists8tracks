-Rodar o script bash "RodaJar" usando o comando:

echo {1..11} | xargs -P 11 -n 1 sh RodarJar.sh



-Onde 1..11 são a quantidade de partes de arquivo e "-P 11" será a quantidade de processos em paralelo.
Esses Valores devem ser alterados dependendo da quantidade de partes do arquivo principal

-A pasta "Arquivos" não esta inclusa.