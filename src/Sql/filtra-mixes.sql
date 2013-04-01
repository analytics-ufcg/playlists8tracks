/*cria a tabela com as mixes/(playlists) filtradas*/
create table mixes_filtro(select id as id_play, name as nome_play, tag_list_cache as tag_list from mixes where(
(tag_list_cache like '%indie%' or tag_list_cache like '%reggae%') and plays_count > 20
and year(updated_at) >= 2010));

/*cria a tabela de tracks apenas com as colunas de interesse, que posteriormente
ajuda muito na performance em queries sob esta mesma*/
create table tracks_less_cols(select id as id_track, year, name as nome_track, performer from tracks);

/*cria índices nas tabelas para melhorar no desempenho e integridade dos dados*/
create unique index endereco on mixes_tracks(id);
create unique index end_track on tracks_less_cols (id_track);
create unique index end_mix on mixes_filtro (id_play);

/*cria a tabela com os joins entre a tabela de mixes filtrada e
a tabela de tracks com as colunas reduzidas, fazendo as correspondências com a tabela
mixes_tracks. obs: esta era a query ques estava dando problema*/
create table mixes_tracks_match(select t.*, f.*
from mixes_filtro f inner join mixes_tracks m on f.id_play = m.mix_id
inner join tracks_less_cols t on t.id_track = m.track_id);

/*seleciona as colunas ordenadas de acordo com a api do echonest para gerar o .csv*/
select m.id_play, m.year, m.performer, m.nome_track, m.id_track, m.tag_list into outfile "~/mixes_tracks_sort.txt" 
from mixes_tracks_match m;

/*seleciona apenas as musicas de playlists com a palavra indie contida na tag_list*/
select m.id_play, m.year, m.performer, m.nome_track, m.id_track, m.tag_list into outfile "~/mixes_tracks_indie.txt" 
from mixes_tracks_match m
where m.tag_list like '%indie%';