SELECT 'p_ID','username','totalVies','totalRemixes','totalFavorites','totalLoves','isRemix','Abstraction','Parallelism','Logic', 'Synchronization','FlowControl','UserInteractivity','DataRepresentation','Mastery','Clones','CustomBlocks','InstancesSprites','scriptRank','totalBlocks'

UNION ALL

SELECT 
	p.p_ID,	
    p.username,	
    p.totalViews,	
    p.totalRemixes,	
    p.totalFavorites,	
    p.totalLoves,	
    p.isRemix,		
    g.Abstraction,	
    g.Parallelism,	
    g.Logic,	
    g.Synchronization,	
    g.FlowControl,	
    g.UserInteractivity,	
    g.DataRepresentation,	
    g.Mastery,	
    g.Clones,	
    g.CustomBlocks,	
    g.InstancesSprites,
    s.scriptRank,
    s.totalBlocks

FROM project AS p 
INNER JOIN grades AS g 
ON g.project_ID = p.p_ID
INNER JOIN script AS s 
ON s.project_ID = p.p_ID
LIMIT 100000
INTO OUTFILE '/var/lib/mysql-files/orders.csv'