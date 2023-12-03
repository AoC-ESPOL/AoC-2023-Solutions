def run():
    restrictions = {
        'red': 12,
        'green': 13,
        'blue': 14 
    }
    
    total = 0
    
    with open('input.txt') as f:
        for line in f:
            semi_colons_index = line.index(':')
            game_id = line[5:semi_colons_index]
            games = line[semi_colons_index+1:].split(';')
            games = [game[1:] for game in games]
            
            add = True
            
            for game in games:
                sub_game = game.split(', ')
                for data in sub_game:
                    data = data.split(' ')
                    color = data[1].strip()
                    amount = int(data[0])
                    
                    if amount > restrictions.get(color, 0):
                        add = False            
                    
            total += int(game_id) if add else 0
    
    print(total)        
            

if __name__ == '__main__':
    run()