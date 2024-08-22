##3 runing time of varies binner

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/airskin/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN airskin.binnigTools.stat.txt

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/Human_gut/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN Human_gut.binnigTools.stat.txt

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/marine/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN marine.binnigTools.stat.txt

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/Mouse_gut/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN Mouse_gut.binnigTools.stat.txt

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/plant_associated/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN plant_associated.binnigTools.stat.txt

    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/strain/Result/pipeline_info
    cat *execution_trace*txt  >  all.execution_trace.txt
    python /aimigene/lianglifeng/pipeline/01.metagenome/modules/MetaFlowX_Plotly_Report/computer.perfomance.transfer.py all.execution_trace.txt METABAT2:MAXBIN2:CONCOCT:METABINNER:SEMIBIN2:BINNY:COMEBIN strain.binnigTools.stat.txt

    #merge running info
    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/paper/MetaflowX_V1_DATA/Fig2
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/Result/pipeline_info/*binnigTools.stat.txt |while read a ;do tail -n +2 $a ;done > p1 
    cat /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/Result/pipeline_info/*binnigTools.stat.txt |head -n 1 > p0 
    cat p0 p1 > 6CAMI.dataset.7binner.running.info.txt
    rm -rf p0 p1 

    le 6CAMI.dataset.7binner.running.info.txt | sed 's/BINNY/binny/g' |sed 's/COMEBIN/COMEBin/g' |sed 's/CONCOCT/CONCOCT/g' |sed 's/MAXBIN2/MaxBin2/g'  | sed 's/METABAT2/MetaBAT2/g' |sed 's/SEMIBIN2/SemiBin2/g' |sed 's/METABINNER/MetaBinner/g' > 6CAMI.dataset.7binner.running.info.rename.txt
    



#FigS5
    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/paper/MetaflowX_V1_DATA/FigS5
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/*Result/02.Contig/*stat | while read a ;do tail -n +2 $a ;done > p1 
    cat /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/*Result/02.Contig/*stat |head -n 1 > p0 
    cat p0 p1 > 6CAMI.dataset.all.sample.contig.stat.txt 
    rm -rf p0 p1 