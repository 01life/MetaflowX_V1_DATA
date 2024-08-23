#Fig2
    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/paper/MetaflowX_V1_DATA/Fig2

    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/01_binner_evaluation/amber/result/*/*_original_binner_AMBER/results.tsv |head -n 1 |while read a ;do head -n 1 $a ;done > p0
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/01_binner_evaluation/amber/result/*/*_original_binner_AMBER/results.tsv | while read a ;do tail -n +2 $a ;done  > p1
    sed 's/Gold standard/GS/g' p1 |sed 's/concoct/CONCOCT/g' |sed 's/comebin/COMEBin/g' |sed 's/maxbin/MaxBin2/g' |sed 's/metabat/MetaBAT2/g' |sed 's/metabinner/MetaBinner/g' |sed 's/semibin2/SemiBin2/g'  > p2
    cat p0 p2 > 6dataset_7binner_single_binner_amber.txt


    #mutlti biner
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/bestbinner/Result/BestBinnerDetector/MetaflowX_binner_DASToolsCombine_Bining_Percentage.*xls |awk -F "/" '{print"tail -n +2 "$0 "  |sed \"s/$/\\t"$7"/g\" >"$7".percentage"}' |sh

    cat *percentage |cut -f 2- > 6dataset_combine_7binner_Bining_Percentage.txt

    sed  -i 1i"binner\tBinType\tavg_Prevalence\tdataset" 6dataset_combine_7binner_Bining_Percentage.txt

    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/bestbinner/Result/BestBinnerDetector/MetaflowX_binner_DASToolsCombine_preSample_avgBinScore_counts.xls |awk -F "/" '{print"tail -n +2 "$0 "  |sed \"s/$/\\t"$7"/g\" >"$7".count"}' |sh

    cat *count  |cut -f 2- > 6dataset_combine_7binner_preSample_avgBinScore_counts.txt
    sed  -i 1i"binner\tBinScore\tavg_counts\tdataset" 6dataset_combine_7binner_preSample_avgBinScore_counts.txt




    grep -wE "concoct_semibin2_metabat|metabat_concoct_semibin2|semibin2_concoct_metabat|semibin2_metabat_concoct" /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/bestbinner/Result/BestBinnerDetector/MetaflowX_binner_DASToolsCombine_Bining_Percentage.*xls |awk -F "/" '{print$7"\t"$NF}' |cut -f 1,3,4,5 > metabat_concoct_semibin2.Bining_Percentage.xls

    grep -wE "concoct_metabat_maxbin_metabinner|concoct_semibin2_maxbin_metabinner|concoct_semibin2_metabat_maxbin|concoct_semibin2_metabat_metabinner|maxbin_concoct_metabinner_metabat|maxbin_concoct_semibin2_metabat|maxbin_concoct_semibin2_metabinner|maxbin_metabat_concoct_metabinner|maxbin_metabat_concoct_semibin2|maxbin_metabat_semibin2_metabinner|maxbin_metabinner_concoct_metabat|maxbin_metabinner_semibin2_concoct|maxbin_metabinner_semibin2_metabat|maxbin_semibin2_concoct_metabat|maxbin_semibin2_metabat_concoct|metabat_concoct_semibin2_metabinner|metabinner_concoct_semibin2_metabat|metabinner_maxbin_concoct_metabat|metabinner_maxbin_concoct_semibin2|metabinner_maxbin_metabat_concoct|metabinner_maxbin_semibin2_concoct|metabinner_maxbin_semibin2_metabat|metabinner_semibin2_concoct_metabat|metabinner_semibin2_metabat_concoct|semibin2_concoct_metabinner_metabat|semibin2_maxbin_concoct_metabat|semibin2_maxbin_concoct_metabinner|semibin2_maxbin_metabinner_metabat|semibin2_metabat_maxbin_metabinner" /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/bestbinner/Result/BestBinnerDetector/MetaflowX_binner_DASToolsCombine_Bining_Percentage.*xls |awk -F "/" '{print$7"\t"$NF}' |cut -f 1,3,4,5 > metabat_concoct_semibin2.Bining_Percentage.xls


#FigS5
    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/paper/MetaflowX_V1_DATA/FigS5
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/*Result/02.Contig/*stat | while read a ;do tail -n +2 $a ;done > p1 
    cat /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/*Result/02.Contig/*stat |head -n 1 > p0 
    cat p0 p1 > 6CAMI.dataset.all.sample.contig.stat.txt 
    rm -rf p0 p1 


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
    cd /aimigene/lianglifeng/01.Project/09.MetaFlowX/paper/MetaflowX_V1_DATA/FigS6
    ls /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/Result/pipeline_info/*binnigTools.stat.txt |while read a ;do tail -n +2 $a ;done > p1 
    cat /aimigene/lianglifeng/01.Project/09.MetaFlowX/06_multi_binner/*/Result/pipeline_info/*binnigTools.stat.txt |head -n 1 > p0 
    cat p0 p1 > 6CAMI.dataset.7binner.running.info.txt
    rm -rf p0 p1 

    le 6CAMI.dataset.7binner.running.info.txt | sed 's/BINNY/binny/g' |sed 's/COMEBIN/COMEBin/g' |sed 's/CONCOCT/CONCOCT/g' |sed 's/MAXBIN2/MaxBin2/g'  | sed 's/METABAT2/MetaBAT2/g' |sed 's/SEMIBIN2/SemiBin2/g' |sed 's/METABINNER/MetaBinner/g' > 6CAMI.dataset.7binner.running.info.rename.txt
