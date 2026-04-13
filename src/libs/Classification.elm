module Classification exposing
    ( ClassificationResult
    , Decision(..)
    , Relevance(..)
    , classificationResultDecoder
    , decisionToString
    , emojiToRelevance
    , isRefusal
    , isStarTag
    , refusalResult
    , relevanceToDecision
    , relevanceToEmoji
    , systemPrompt
    , userPrompt
    )

import Json.Decode as Decode exposing (Decoder)


type Relevance
    = OneStar
    | TwoStars
    | ThreeStars
    | FourStars
    | FiveStars


type Decision
    = Include
    | Exclude


type alias ClassificationResult =
    { relevance : Relevance
    , reasoning : String
    , note : String
    , deathAfterTherapy : Bool
    }


relevanceToEmoji : Relevance -> String
relevanceToEmoji relevance =
    case relevance of
        OneStar ->
            "⭐"

        TwoStars ->
            "⭐⭐"

        ThreeStars ->
            "⭐⭐⭐"

        FourStars ->
            "⭐⭐⭐⭐"

        FiveStars ->
            "⭐⭐⭐⭐⭐"


emojiToRelevance : String -> Maybe Relevance
emojiToRelevance emoji =
    case emoji of
        "⭐" ->
            Just OneStar

        "⭐⭐" ->
            Just TwoStars

        "⭐⭐⭐" ->
            Just ThreeStars

        "⭐⭐⭐⭐" ->
            Just FourStars

        "⭐⭐⭐⭐⭐" ->
            Just FiveStars

        _ ->
            Nothing


relevanceToDecision : Relevance -> Decision
relevanceToDecision relevance =
    case relevance of
        OneStar ->
            Exclude

        TwoStars ->
            Exclude

        ThreeStars ->
            Include

        FourStars ->
            Include

        FiveStars ->
            Include


decisionToString : Decision -> String
decisionToString decision =
    case decision of
        Include ->
            "INCLUDE"

        Exclude ->
            "EXCLUDE"


isStarTag : String -> Bool
isStarTag tag =
    List.member tag [ "⭐", "⭐⭐", "⭐⭐⭐", "⭐⭐⭐⭐", "⭐⭐⭐⭐⭐" ]


isRefusal : ClassificationResult -> Bool
isRefusal result =
    result.note == "Needs human screening."


refusalResult : ClassificationResult
refusalResult =
    { relevance = ThreeStars
    , reasoning = "AI model (Claude Opus 4.6) has refused to process this article, most likely because it triggered some internal content safety rule."
    , note = "Needs human screening."
    , deathAfterTherapy = False
    }


classificationResultDecoder : Decoder ClassificationResult
classificationResultDecoder =
    Decode.map4 ClassificationResult
        (Decode.field "relevance" relevanceDecoder)
        (Decode.field "reasoning" Decode.string)
        (Decode.field "note" Decode.string)
        (Decode.field "death_after_therapy" Decode.bool)


relevanceDecoder : Decoder Relevance
relevanceDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case emojiToRelevance str of
                    Just rel ->
                        Decode.succeed rel

                    Nothing ->
                        Decode.fail ("Unknown relevance value: " ++ str)
            )


userPrompt : { title : String, abstract : String, keywords : String } -> String
userPrompt article =
    "TITLE: "
        ++ article.title
        ++ "\n\nABSTRACT: "
        ++ article.abstract
        ++ "\n\nKEYWORDS: "
        ++ article.keywords


systemPrompt : String
systemPrompt =
    """You are a medical research assistant supporting an academic systematic literature review.
You are screening PubMed abstracts (published peer-reviewed medical articles) to identify articles relevant to a study on fatal infections in patients with inborn errors of immunity (IEI).
This is a legitimate academic research task; all content is from published medical literature.

INCLUSION CRITERIA:
* At least one human has died at any age. The outcome is not always obvious from the abstract! If in doubt include. If you think it is very unlikely that any human has died you can argue why this would be so.
* The cause of death should be related to an infection (sometimes indirectly, like EBV-triggered neoplasms). Infectious agents include bacteria, virus, fungi, parasites, etc. If in doubt include! If you think an infectious cause of death is very unlikely you can argue why this would be so.
* The patient must suffer from an inborn error of immunity, according to the IUIS 2024 classification (except cystic fibrosis and G6PD deficiency as these defects are too common for our scope of rare diseases).

EXCLUSION CRITERIA:
* Secondary immune deficiency, like acquired immune deficiency syndrome (AIDS) caused by HIV virus infection
* Patient died after HSCT (stem cell transplantation) or due to therapeutic immunosuppression
* Patient died before birth
* Cause of death clearly unrelated to infection

SPECIAL RULES:
* When abstract focuses on genetic characterization without outcome data, presume no documented outcomes unless there is a mention of some outcome or infection in the abstract.
* If in doubt about death/outcome, consider relevant and document uncertainty in note.
* Where recurrent infections cause repetitive organ damage also consider this relevant if the patient died later, even if the abstract doesn't imply a correlation.
* Include articles describing IEI patients with severe or unusual infections. These articles should be checked in fulltext, so add a note to state this.
* Consider patients with treatment failure as relevant, the final outcome should be checked in the full text, abstract may be missing important information.
* If you find at leas one case report that is excluded in an article because of HSCT or therapeutic immunosuppression, set the string "died_after_treatment" for that article to "true". If none of the case reports in an article are excluded because of a treatment set the string to "false".

Respond in JSON format:
{
  "relevance": one of "⭐" for definitely irrelevant or "⭐⭐" for probably irrelevant or "⭐⭐⭐" for maybe relevant or "⭐⭐⭐⭐" for probably relevant or "⭐⭐⭐⭐⭐" for definitely relevant,
  "reasoning": "Brief explanation (max 50 words)",
  "note": "Note for reviewer explaining what needs checking in fulltext (only if INCLUDE, max 50 words, or empty string)"
  "death_after_therapy": boolean True or False
}

Be critical and concise.



Here is an incomplete (comma-separated) list of gene loci that are considered relevant for IEI:
11q23,14q32,22q11.2,ACD,ACP5,ACTB,ADA,ADA2,ADAM17,ADAR1,AICDA,AIRE,AK2,ALPI,ALPK1,AP1S3,AP3B1,AP3D1,APOL1,ARHGEF1,ARPC1B,ARPC5,ATAD3A,ATG4A,ATM,ATP6AP1,B2M,BACH2,BCL10,BCL11B,BLM,BLNK,BRCA1,BRCA2,BRIP1,BTK,C1Q,C1QA,C1QB,C1QC,C1R,C1S,C2,C2orf69,C3,C4A,C4B,C5,C6,C7,C8A,C8B,C8G,C9,CARD11,CARD14,CARD9,CARMIL2,CASP10,CASP8,CBLB,CCBE1,CCR2,CD19,CD20,CD21,CD247,CD27,CD274,CD28,CD3D,CD3E,CD3G,CD40,CD40LG,CD46,CD55,CD59,CD70,CD79A,CD79B,CD81,CD8A,CDC42,CDCA7,CEBPE,CFB,CFD,CFH,CFHR1,CFHR2,CFHR3,CFHR4,CFHR5,CFI,CFP,CHD7,CHUK,CIB1,CIITA,CLCN7,CLPB,COPA,COPG1,CORO1A,CRACR2A,CSF2RA,CSF2RB,CSF3R,CTC1,CTLA4,CTNNBL1,CTPS1,CTSC,CXCR2,CXCR4,CYBA,CYBB,CYBC1,DBF4,DBR1,DCLRE1B,DCLRE1C,DEF6,10p13-p14,DIAPH1,DKC1,DNAJC21,DNASE1L3,DNASE2,DNMT3B,DOCK11,DOCK2,DOCK8,DPP9,DUT,EFL1,ELANE,ELF4,EPG5,ERBIN,ERCC4,ERCC6L2,ERN1,EXTL3,FAAP24,FADD,FANCA,FANCB,FANCC,FANCD2,FANCE,FANCF,FANCI,FANCL,FANCM,FAT4,FCGR3A,FCHO1,FCN3,FERMT1,FERMT3,FLT3LG,FNIP1,FOXI3,FOXN1,FOXP3,FPR1,G6PC3,G6PT1,GATA2,GFI1,GIMAP5,GIMAP6,GINS1,GINS4,GTF3A,HAVCR2,HAX1,HCK,HELLS,HMOX1,HYOU1,ICOS,ICOSLG,IFIH1,IFNAR1,IFNAR2,IFNG,IFNGR1,IFNGR2,IGHM,IGKC,IGLL1,IKBKB,IKBKE,IKBKG,IKZF1,IKZF2,IKZF3,IL10,IL10RA,IL10RB,IL12B,IL12RB1,IL12RB2,IL17F,IL17RA,IL17RC,IL18BP,IL1R1,IL1RN,IL21,IL21R,IL23R,IL27RA,IL2RA,IL2RB,IL2RG,IL36RN,IL6R,IL6ST,IL7R,INO80,IRAK1,IRAK4,IRF1,IRF2BP2,IRF3,IRF4,IRF7,IRF8,IRF9,ISG15,ITCH,ITGB2,ITK,ITPKB,ITPR3,JAGN1,JAK1,JAK3,KARS1,KDM6A,KMT2A,KMT2D,LACC1,LAMTOR2,LAT,LCK,LCP2,LIG1,LIG4,LPIN2,LRBA,LSM11,LY96,LYN,LYST,MAD2L2,MAGT1,MALT1,MAN2B2,MAP1LC3B2,MAP3K14,MAPK8,MASP2,MCM10,MCM4,MCTS1,MECOM,MEFV,MOGS,MRTFA,MSH6,MSN,MTHFD1,MVK,MYD88,MYSM1,NA,NBAS,NBEAL2,NBN,NCF1,NCF2,NCF4,NCKAP1L,NCSTN,NFAT5,NFATC1,NFATC2,NFE2L2,NFKB1,NFKB2,NFKBIA,NHEJ1,NLRC4,NLRP1,NLRP12,NLRP3,NOD2,NOLA2,NOLA3,NOS2,NSMCE3,NUDCD3,OAS1,OAS2,ORAI1,OSTM1,OTULIN,PALB2,PARN,PAX1,PAX5,PDCD1,PEPD,PGM3,PIK3CD,PIK3CG,PIK3R1,PLCG1,PLCG2,PLEKHM1,PMS2,PMVK,PNP,POLA1,POLD1,POLD2,POLD3,POLE1,POLE2,POLR3A,POLR3C,POLR3F,POMP,POU2AF1,PRF1,PRIM1,PRKCD,PRKDC,PSEN,PSENEN,PSMB10,PSMB4,PSMB8,PSMB9,PSMD12,PSMG2,PSTPIP1,PTCRA,PTEN,PTPRC,RAB27A,RAC2,RAD50,RAD51,RAD51C,RAG1,RAG2,RANBP2,RASGRP1,RBCK1,RECQL4,REL,RELA,RELB,RFWD3,RFX5,RFXANK,RFXAP,RHBDF2,RHOG,RHOH,RIPK1,RIPK3,RMRP,RNASEH2A,RNASEH2B,RNASEH2C,RNASEL,RNF168,RNF31,RNU4ATAC,RNU7-1,RORC,RPSA,RTEL1,SAMD9,SAMD9L,SAMHD1,SASH3,SBDS,SEC61A1,SEMA3E,SERPING1,SGPL1,SH2B3,SH2D1A,SH3BP2,SH3KBP1,SHARPIN,SKIV2L,SLC19A1,SLC29A3,SLC35C1,SLC39A7,SLC46A1,SLC7A7,SLX4,SMARCAL1,SMARCD2,SNORA31,SNX10,SOCS1,SP110,SPI1,SPINK5,SPPL2A,SRP19,SRP54,SRP72,SRPRA,STAT1,STAT2,STAT3,STAT4,STAT5B,STAT6,STIM1,STING1,STK4,STN1,STX11,STXBP2,SYK,TAP1,TAP2,TAPBP,TAZ,TBK1,TBX1,TBX21,TCF3,TCIRG1,TCN2,TERC,TERT,TET2,TFRC,TGFB1,TGFBR1,TGFBR2,THBD,TICAM1,TINF2,TIRAP,TLR3,TLR4,TLR7,TLR8,TMC6,TMC8,TNFAIP3,TNFRSF11A,TNFRSF13B,TNFRSF13C,TNFRSF1A,TNFRSF4,TNFRSF6,TNFRSF9,TNFSF11,TNFSF12,TNFSF13,TNFSF6,TNFSF9,TOP2B,TP53,TPP2,TRAC,TRAF3,TRAF3IP2,TREX1,TRIM22,TRNT1,TTC37,TTC7A,TYK2,UBE2T,UNC13D,UNC93B1,UNG,USB1,USP18,VPS13B,VPS45,WAS,WDR1,WIPF1,WRAP53,XIAP,XRCC2,XRCC9,ZAP70,ZBTB24,ZNF341,ZNFX1

And here are some more relevant disease names:

Adenosine deaminase,Agammaglobulinemia,BAFF-R,BIRC4,CD154,CECR1,Chediak-Higashi,CHS,Common gamma chain,DiGeorge,GP91,gp91phox,Hermansky-Pudlak,IKBA,IPEX,IκBα,Kindlin-1,Kindlin-3,MUNC13-4,MUNC18-2,NEMO,p40phox,p47phox,p67phox,SAP,TACI,TNFRSF7,Wiskott-Aldrich,HEM1,primary immunodeficiencies,aplastic thymus,MHC deficiency,CD3z,IL7Ra,SLP76,Activated RAC2,FCHO1,ICOLG,IKBKB,MAN2B2,NFATC1,TCRa,ICF3,ICF1,GINS1,GINS4,centromeric instability AND facial anomalies,MCM4,NSMCE3,POLE1,POLE2,CHARGE syndrome,MOPD1,IL6 receptor,IL6 signal transducer,Job syndrome,HIES,Loeys Dietz syndrome,Methylene-tetrahydrofolate dehydrogenase 1,hereditary folate,EDA-ID,CRACR2A,ORAI-1,Hennekam-lymphangiectasia-lymphedema syndrome,Vici syndrome,Kabuki Syndrome,Kabuki Syndrome,HOIL1,Tricho-hepato-enteric syndrome,Bone marrow failure syndrome,PTCRA,Igb,m heavy chain,PIK3CD,PIK3R1,SLC39A7,E47,Hoffman syndrome,ARHGEF1,IKAROS,NFKB2,PIK3CG,PIK3R1,POU2AF1,SEC61A1,SH3KBP1,BAFF receptor,TWEAK,KARS1,CTNNBL1,TNFSF13,Ig heavy chain,Transient hypogammaglobulinemia of infancy,antibody deficiency,RHOG,Lysinuric protein intolerance,Syntaxin 11,Munc18-2,Munc13-4,Hermansky-Pudlak syndrome,GIMAP6,FERMT1,Ikaros,CD122,ARPC5,IRE1a,PLCG1,Tripeptidyl-Peptidase II,ELF,iRHOM,autoimmune lymphoproliferative syndrome,IL-27RA,4-1 BBL,G-CSF receptor,DBF4,Schwachman Diamond syndrome,GFI 1,HYOU1,P14/LAMTOR2,SRP19,SRPRA,Clericuzio,X-linked neutropenia,X-linked myelodysplasia,b actin,Rac 2,Autosomal recessive CGD,p22phox,EROS,Autosomal recessive CGD,Congenital pulmonary alveolar proteinosis,IFN-g,IL-23,IL-12,IL-12Rb2,MCTS1,RORgt,TBX21,P1104A,CIB1,EVER1,EVER2,CD16,OAS2,polymerase III,polymerase 3,RNASEL,GTF3A,IKBKE,SNORA31,MAPK8,IRAK4,TLR8,Trypanosomiasis,osteopetrosis,Acute liver failure,hidradenitis suppurativa,Spondyloenchondro-dysplasia,ADAR1,AGS6,C2orf69,CDC42,LSM11,MIS-C,PIMS,interferonopathy,RNASEH2A,RNASEH2B,RNASEH2C,RNU7-1,SAMHD1,AGS2,AGS3,AGS4,AGS5,STAT2,STING vasculopathy,STING disease,Palmoplantar carcinoma,recurrent respiratory papillomatosis,PVMK,ALPI,ROSAH,AP1S3,LIRSA,lavli,Otulipenia,ORAS,Otulin,PRAAS,CANDLE,PSMD12,PAPA,ENT3,Periodontal Ehlers Danlos,C8a,C8b,C8g,Membrane Cofactor Protein,Membrane Attack Complex Inhibitor,Factor B,Factor B,Factor H,MASP2,Dyskeratosis congenita,DUT,EVI1,MECOM,Coats,Fanconi Anemia,Ataxia Pancytopenia Syndrome,Coronin-1A deficiency,gc deficiency,JAK3 deficiency,LAT deficiency,CD45 deficiency,Adenosine deaminase deficiency,Reticular dysgenesis,Artemis deficiency,DNA ligase IV deficiency,Cernunnos/XLF deficiency,DNA PKcs deficiency,RAG1 deficiency,RAG2 deficiency,MHC deficiency,BCL10 deficiency,CD40 deficiency,CD40 ligand deficiency,CD8 deficiency,CHUK deficiency,DOCK2 deficiency,DOCK8 deficiency,ICOS deficiency,IKBKB,Ikaros deficiency,HELIOS deficiency,IL-21 deficiency,IL-21R deficiency,ITK deficiency,LCK deficiency,MALT1 deficiency,NIK deficiency,Moesin deficiency,PRIM1,Omenn Syndrome,c-Rel deficiency,RelB deficiency,Rhoh Deficiency,SASH3 deficiency,MST1 deficiency,TFRC deficiency,OX40 deficiency,ZAP-70,ARPC1B deficiency,Wiskott-Aldrich syndrome,WIP deficiency,Ataxia-telangiectasia,Bloom Syndrome,Ligase I deficiency,MCM10 deficiency,Nijmegen breakage syndrome,PMS2 Deficiency,X-linked reticulate pigmentary,RAD50 deficiency,RNF168 deficiency,11q deletion,10p13 deletion,FOXN1,22q11ds,TBX1 deficiency,DiGeorge/velocardiofacial syndrome,Immunoskeletal dysplasia with neurodevelopmental abnormalities,MYSM1 deficiency,Cartilage hair hypoplasia,Schimke Immuno-osseous Dysplasia,ERBIN deficiency,PGM3 deficiency,Comel-Netherton syndrome,Transcobalamin 2 deficiency,EDA-ID,ITPR3,STIM1 deficiency,BCL11B deficiency,CD28 deficiency,DIAPH1 deficiency,AIOLOS deficiency,Wiedemann-Steiner syndrome,Purine nucleoside phosphorylase deficiency,HOIP deficiency,Hepatic veno-occlusive disease with immunodeficiency,STAT5b deficiency,Immunodeficiency with multiple intestinal atresias,FLT3L deficiency,SGPL1 deficiency,BLNK deficiency,X-linked agammaglobulinemia,Iga deficiency,FNIP1 deficiency,Pu.1 deficiency,PAX5 deficiency,ATP6AP1 deficiency,CD19 deficiency,CD20 deficiency,CD21 deficiency,CD81 deficiency,IRF2BP2 deficiency,Mannosyl-oligosaccharide glucosidase deficiency,NFKB1 deficiency,PTEN Deficiency,RAC2 deficiency,TACI deficiency,TRNT1 deficiency,Common variable immune deficiency,AID deficiency,INO80,MSH6,UNG deficiency,Kappa chain deficiency,Selective IgM  deficiency,Isolated IgG subclass deficiency,Selective IgA deficiency,DPP9 deficiency,FAAP24 deficiency,Perforin deficiency,Chediak-Higashi syndrome,Griscelli syndrome, type 2,BACH2 deficiency,CTLA4 deficiency,DEF6 deficiency,IPEX, immune dysregulation, polyendocrinopathy, enteropathy X-linked,CD25 deficiency,LRBA deficiency,NBEAL2 deficiency,STAT3 GOF,APECED,CBLB deficiency,PD-L1 deficiency,GIMAP5 deficiency,ITCH deficiency,JAK1 GOF,LACC1 deficiency,NFAT1 deficiency,PD1 deficiency,Prolidase deficiency,SH2B3 deficiency,SOCS1 deficiency,TLR7 deficiency,UNC93B1 deficiency,DOCK11 deficiency,IL-10 deficiency,IL-10Ra deficiency,IL-10Rb deficiency,NFAT5 haploinsufficiency,RIPK1 deficiency,TGFB1 deficiency,FADD deficiency,ALPS-FAS,ALPS-FASLG,RLTPR,CD27 deficiency,CD70 deficiency,CTPS1 deficiency,MAGT1 deficiency,PRKCD deficiency,RASGRP1 deficiency,SAP deficiency,TET2 deficiency,CD137 deficiency,XIAP deficiency,Specific granule deficiency,3-Methylglutaconic aciduria,CXCR2 deficiency,Elastase deficiency,G6PC3 deficiency,Glycogen storage disease type 1b,Kostmann Disease,JAGN1 deficiency,SMARCD2 deficiency,SRP54 deficiency,Barth Syndrome,Cohen syndrome,VPS45 deficiency,CCR2 deficiency,Cystic fibrosis,Papillon-Lefèvre Syndrome,Leukocyte adhesion deficiency,Localized juvenile periodontitis,LAD1,MKL1,LAD2,WDR1 deficiency,X-linked chronic granulomatous disease,GATA2 deficiency,IRF1 deficiency,IRF8 deficiency,ISG15 deficiency,JAK1,SPPL2a deficiency,STAT1 deficiency,WHIM  syndrome,MDA5 deficiency,IFNAR1 deficiency,IFNAR2 deficiency,IRF7 deficiency,IRF9 deficiency,NOS2 deficiency,STAT2 deficiency,ZNFX1 deficiency,ATG4A,DBR1 deficiency,IRF3 deficiency,MAP1LC3B2,RIPK3 deficiency,TBK1 deficiency,TRIF deficiency,TLR3 deficiency,CARD9 deficiency,IL-17F deficiency,IL-17RA deficiency,IL-17RC deficiency,STAT1 GOF,ACT1 deficiency,MD2 deficiency,TLR4 deficiency,IRAK1 deficiency,IRAK4 deficiency,MyD88 deficiency,TIRAP deficiency,Isolated congenital asplenia,Acute necrotizing encephalopathy,ICA,IL-18BP deficiency,ADA2 deficiency,ATAD3A deficiency,DNASE1L3 deficiency,DNASE2 deficiency,Aicardi-Goutieres syndrome,Pansclerotic morphea,AGS,USP18 deficiency,Familial Mediterranean fever,Mevalonate kinase deficiency,macrophage activating syndrome,NLRP1 deficiency,Familial cold autoinflammatory syndrome 2,Muckle-Wells syndrome,Familial cold autoinflammatory syndrome 1,NOMID,ADAM17 deficiency,CAMPS,Tim-3 deficiency,DIRA,DITRA,HEM1 deficiency,Blau syndrome,Otulin deficiency,familial cold autoinflammatory ,PRAID,CANDLE,Cherubism,SHARPIN deficiency,A20 deficiency,TNF receptor-associated periodic syndrome,TRIM22,C1r deficiency,C1s deficiency,C2 deficiency,C3 deficiency,Complete C4 deficiency,C5 deficiency,C6 deficiency,C7 deficiency,C9 deficiency,CD55 deficiency,Factor D deficiency,Factor H deficiency,Factor I deficiency,Properdin deficiency,Ficolin 3 deficiency,C1 inhibitor deficiency,Thrombomodulin deficiency"""
