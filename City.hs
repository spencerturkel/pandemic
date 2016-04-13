module City where

citiesConnected :: City -> City -> Bool
citiesConnected x y = x `elem` connectionsFromCity y

-- Manually enumerated.
data City
  = SanFrancisco
  | Chicago
  | Toronto
  | Atlanta
  | Washington
  | NewYork
  | London
  | Madrid
  | Paris
  | Essen
  | Milan
  | StPetersburg
  | LosAngeles
  | MexicoCity
  | Miami
  | Bogota
  | Lima
  | Santiago
  | SaoPaulo
  | BuenosAires
  | Lagos
  | Kinshasha
  | Johannesburg
  | Khartoum
  | Algiers
  | Cairo
  | Istanbul
  | Baghdad
  | Riyadh
  | Moscow
  | Tehran
  | Karachi
  | Mumbai
  | Delhi
  | Kolkata
  | Chennai
  | Bangkok
  | Jakarta
  | HoChiMinhCity
  | HongKong
  | Shanghai
  | Beijing
  | Seoul
  | Tokyo
  | Osaka
  | Taipei
  | Manila
  | Sydney
  deriving (Show, Read, Eq)

-- Manually enumerated, using a clockwise scanning pattern from 12 o'clock.
connectionsFromCity :: City -> [City]
connectionsFromCity SanFrancisco = [ Chicago
                                   , LosAngeles
                                   , Manila
                                   , Tokyo ]
connectionsFromCity Chicago = [ Toronto
                              , Atlanta
                              , MexicoCity
                              , LosAngeles
                              , SanFrancisco ]
connectionsFromCity Toronto = [ NewYork
                              , Washington
                              , Chicago ]
connectionsFromCity Atlanta = [ Washington
                              , Miami
                              , Chicago ]
connectionsFromCity Washington = [ NewYork
                                 , Miami
                                 , Atlanta
                                 , Toronto ]
connectionsFromCity NewYork = [ London
                              , Madrid
                              , Washington
                              , Toronto ]
connectionsFromCity London = [ Essen
                             , Paris
                             , Madrid
                             , NewYork ]
connectionsFromCity Madrid = [ Paris
                             , Algiers
                             , SaoPaulo
                             , NewYork
                             , London ]
connectionsFromCity Paris = [ Essen
                            , Milan
                            , Algiers
                            , Madrid
                            , London ]
connectionsFromCity Essen = [ StPetersburg
                            , Milan
                            , Paris
                            , London ]
connectionsFromCity Milan = [ Istanbul
                            , Paris
                            , Essen ]
connectionsFromCity StPetersburg = [ Moscow
                                   , Istanbul
                                   , Essen ]
connectionsFromCity LosAngeles = [ Chicago
                                 , MexicoCity
                                 , Sydney
                                 , SanFrancisco ]
connectionsFromCity MexicoCity = [ Chicago
                                 , Miami
                                 , Bogota
                                 , Lima
                                 , LosAngeles ]
connectionsFromCity Miami = [ Washington
                            , Bogota
                            , MexicoCity
                            , Atlanta ]
connectionsFromCity Bogota = [ SaoPaulo
                             , BuenosAires
                             , Lima
                             , MexicoCity
                             , Miami ]
connectionsFromCity Lima = [ Bogota
                           , Santiago
                           , MexicoCity ]
connectionsFromCity Santiago = [ Lima ]
connectionsFromCity SaoPaulo = [ Madrid
                               , Lagos
                               , BuenosAires
                               , Bogota ]
connectionsFromCity BuenosAires = [ SaoPaulo
                                  , Bogota ]
connectionsFromCity Lagos = [ Khartoum
                            , Kinshasha
                            , SaoPaulo ]
connectionsFromCity Kinshasha = [ Khartoum
                                , Johannesburg
                                , Lagos ]
connectionsFromCity Johannesburg = [ Khartoum
                                   , Kinshasha ]
connectionsFromCity Khartoum = [ Johannesburg
                               , Kinshasha
                               , Lagos
                               , Cairo ]
connectionsFromCity Algiers = [ Istanbul
                              , Cairo
                              , Madrid
                              , Paris ]
connectionsFromCity Cairo = [ Istanbul
                            , Baghdad
                            , Riyadh
                            , Khartoum
                            , Algiers ]
connectionsFromCity Istanbul = [ StPetersburg
                               , Moscow
                               , Baghdad
                               , Cairo
                               , Algiers
                               , Milan ]
connectionsFromCity Baghdad = [ Tehran
                              , Karachi
                              , Riyadh
                              , Cairo
                              , Istanbul ]
connectionsFromCity Riyadh = [ Karachi
                             , Cairo
                             , Baghdad ]
connectionsFromCity Moscow = [ Tehran
                             , Istanbul
                             , StPetersburg ]
connectionsFromCity Tehran = [ Delhi
                             , Baghdad
                             , Moscow ]
connectionsFromCity Karachi = [ Delhi
                              , Mumbai
                              , Riyadh
                              , Baghdad ]
connectionsFromCity Mumbai = [ Delhi
                             , Chennai
                             , Karachi ]
connectionsFromCity Delhi = [ Kolkata
                            , Chennai
                            , Mumbai
                            , Karachi
                            , Tehran ]
connectionsFromCity Kolkata = [ HongKong
                              , Bangkok
                              , Chennai
                              , Delhi ]
connectionsFromCity Chennai = [ Delhi
                              , Kolkata
                              , Bangkok
                              , Mumbai ]
connectionsFromCity Bangkok = [ HongKong
                              , HoChiMinhCity
                              , Jakarta
                              , Chennai
                              , Kolkata ]
connectionsFromCity Jakarta = [ HoChiMinhCity
                              , Sydney
                              , Chennai
                              , Bangkok ]
connectionsFromCity HoChiMinhCity = [ Manila
                                    , Jakarta
                                    , Bangkok
                                    , HongKong ]
connectionsFromCity HongKong = [ Shanghai
                               , Taipei
                               , Manila
                               , HoChiMinhCity
                               , Bangkok
                               , Kolkata ]
connectionsFromCity Shanghai = [ Seoul
                               , Tokyo
                               , Taipei
                               , HongKong
                               , Beijing ]
connectionsFromCity Beijing = [ Seoul
                              , Shanghai ]
connectionsFromCity Seoul = [ Tokyo
                            , Shanghai
                            , Beijing ]
connectionsFromCity Tokyo = [ SanFrancisco
                            , Osaka
                            , Shanghai
                            , Seoul ]
connectionsFromCity Osaka = [ Tokyo
                            , Taipei ]
connectionsFromCity Taipei = [ Osaka
                             , Manila
                             , HongKong
                             , Shanghai ]
connectionsFromCity Manila = [ Taipei
                             , SanFrancisco
                             , Sydney
                             , HoChiMinhCity
                             , HongKong ]
connectionsFromCity Sydney = [ LosAngeles
                             , Jakarta
                             , Manila ]
