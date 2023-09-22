
# A Simple Food Environment Data Pipeline from OpenStreetMaps to ONS Statistics

## Workflow diagram

This is generated automatically–it should render properly on GitHub.

``` mermaid
graph LR
  subgraph legend
    x7420bd9270f8d27d([""Up to date""]):::uptodate --- xbf4603d6c2c2ad6b([""Stem""]):::none
    xbf4603d6c2c2ad6b([""Stem""]):::none --- xf0bce276fe2b9d3e>""Function""]:::none
  end
  subgraph Graph
    x7a783f7888920e8e>"sf_to_latlon"]:::uptodate --> xb7fdee14fd025706>"save_food_files"]:::uptodate
    xb8b29bd2f630d625(["foodspace_restaurant"]):::uptodate --> xcad63a2bd74529a4(["foodspace_restaurant_distances"]):::uptodate
    x86775e87634515f6>"get_db_centroid_walkdistance"]:::uptodate --> xcad63a2bd74529a4(["foodspace_restaurant_distances"]):::uptodate
    xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate --> xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate
    x4a04883f7c14b3e8(["specialty_amenities"]):::uptodate --> xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate
    x4869bfa6f6a8fafa(["specialty_shops"]):::uptodate --> xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate
    x655fb7e96e12f421>"compute_and_save_stats"]:::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    x0fb2f1e001a53162(["da_ons_sli"]):::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    xffb3b12d95480031(["dbpops"]):::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    xf4775bcb7e0d3ab4(["foodspace_fast_food"]):::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    x54d555ded1a4ee65(["foodspace_fastfood_distances"]):::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate
    x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    xc72c1097e11c1d14(["foodspace_stats_fastfood"]):::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    xffe9856255665d72>"save_consolidated_food_stats"]:::uptodate --> x106efd0886352d6f(["foodspace_stats_consolidated"]):::uptodate
    x655fb7e96e12f421>"compute_and_save_stats"]:::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    x0fb2f1e001a53162(["da_ons_sli"]):::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    xffb3b12d95480031(["dbpops"]):::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    xc5333862c577abd7(["foodspace_convenience"]):::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    x8fd8f118089dbaa3(["foodspace_convenience_distances"]):::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> x4ebfefa8dd11fe37(["foodspace_stats_convenience"]):::uptodate
    xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate --> xb8b29bd2f630d625(["foodspace_restaurant"]):::uptodate
    xca4719305968821b(["restaurant_amenities"]):::uptodate --> xb8b29bd2f630d625(["foodspace_restaurant"]):::uptodate
    x655fb7e96e12f421>"compute_and_save_stats"]:::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    x0fb2f1e001a53162(["da_ons_sli"]):::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    xffb3b12d95480031(["dbpops"]):::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    x701371b8b50ef85c(["foodspace_specialty_distances"]):::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> x062fbca3195f636d(["foodspace_stats_specialty"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate
    x655fb7e96e12f421>"compute_and_save_stats"]:::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    x0fb2f1e001a53162(["da_ons_sli"]):::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    xffb3b12d95480031(["dbpops"]):::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    x4727ed3549c0d2ea(["foodspace_grocery"]):::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    x8b999716c5105a17(["foodspace_grocery_distances"]):::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> x2c21dbbb248b019a(["foodspace_stats_grocery"]):::uptodate
    x4e63b4444947a83a(["convenience_shops"]):::uptodate --> xc5333862c577abd7(["foodspace_convenience"]):::uptodate
    xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate --> xc5333862c577abd7(["foodspace_convenience"]):::uptodate
    xc5333862c577abd7(["foodspace_convenience"]):::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    xf4775bcb7e0d3ab4(["foodspace_fast_food"]):::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    x4727ed3549c0d2ea(["foodspace_grocery"]):::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    xb8b29bd2f630d625(["foodspace_restaurant"]):::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    xb7fdee14fd025706>"save_food_files"]:::uptodate --> x8e0a00ba76c09b17(["save_foodspace"]):::uptodate
    x4727ed3549c0d2ea(["foodspace_grocery"]):::uptodate --> x8b999716c5105a17(["foodspace_grocery_distances"]):::uptodate
    x86775e87634515f6>"get_db_centroid_walkdistance"]:::uptodate --> x8b999716c5105a17(["foodspace_grocery_distances"]):::uptodate
    x655fb7e96e12f421>"compute_and_save_stats"]:::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    x0fb2f1e001a53162(["da_ons_sli"]):::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    xffb3b12d95480031(["dbpops"]):::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    xb8b29bd2f630d625(["foodspace_restaurant"]):::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    xcad63a2bd74529a4(["foodspace_restaurant_distances"]):::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    x55603c1aca1d7aa0(["ons_gen3_shp"]):::uptodate --> xa2e778cbf5093569(["foodspace_stats_restaurant"]):::uptodate
    xbbd3e9b50b94bbc2(["foodspace_specialty"]):::uptodate --> x701371b8b50ef85c(["foodspace_specialty_distances"]):::uptodate
    x86775e87634515f6>"get_db_centroid_walkdistance"]:::uptodate --> x701371b8b50ef85c(["foodspace_specialty_distances"]):::uptodate
    xf4775bcb7e0d3ab4(["foodspace_fast_food"]):::uptodate --> x54d555ded1a4ee65(["foodspace_fastfood_distances"]):::uptodate
    x86775e87634515f6>"get_db_centroid_walkdistance"]:::uptodate --> x54d555ded1a4ee65(["foodspace_fastfood_distances"]):::uptodate
    x13a9e9c5691a6d51(["grocery_shops"]):::uptodate --> x4727ed3549c0d2ea(["foodspace_grocery"]):::uptodate
    xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate --> x4727ed3549c0d2ea(["foodspace_grocery"]):::uptodate
    x6fd4455e84aa3cc2(["fast_food_amenities"]):::uptodate --> xf4775bcb7e0d3ab4(["foodspace_fast_food"]):::uptodate
    xa5a1a0fb6b9cd1c9(["ons_gen3_buffer"]):::uptodate --> xf4775bcb7e0d3ab4(["foodspace_fast_food"]):::uptodate
    xc5333862c577abd7(["foodspace_convenience"]):::uptodate --> x8fd8f118089dbaa3(["foodspace_convenience_distances"]):::uptodate
    x86775e87634515f6>"get_db_centroid_walkdistance"]:::uptodate --> x8fd8f118089dbaa3(["foodspace_convenience_distances"]):::uptodate
    x99d03ba90471615e>"mapcheck"]:::uptodate --> x99d03ba90471615e>"mapcheck"]:::uptodate
  end
  classDef uptodate stroke:#000000,color:#ffffff,fill:#354823;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
  linkStyle 0 stroke-width:0px;
  linkStyle 1 stroke-width:0px;
  linkStyle 67 stroke-width:0px;
```
